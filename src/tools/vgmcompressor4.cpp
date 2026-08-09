#include <algorithm>
#include <array>
#include <cctype>
#include <cstdint>
#include <filesystem>
#include <iomanip>
#include <iostream>
#include <limits>
#include <optional>
#include <sstream>
#include <stdexcept>
#include <string>
#include <string_view>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <vector>

namespace {

constexpr std::size_t kBankSize = 0x4000;
constexpr std::size_t kBankTrailerSize = 4;
constexpr std::size_t kMaxLiteralWrites = 0xff;
constexpr std::array<std::size_t, 3> kPhraseLengths = {8, 6, 4};

using Bytes = std::vector<std::uint8_t>;

struct Frame {
  std::array<Bytes, 2> ports;
};

struct Phrase {
  std::uint64_t data = 0;
  std::uint8_t size = 0;

  bool operator==(const Phrase &) const = default;
};

struct PhraseHash {
  std::size_t operator()(const Phrase &phrase) const noexcept {
    auto value = static_cast<std::size_t>(phrase.data ^ (phrase.data >> 32));
    return value ^ (static_cast<std::size_t>(phrase.size) * 0x9e3779b9U);
  }
};

struct PhraseLess {
  bool operator()(const Phrase &left, const Phrase &right) const noexcept;
};

struct Segment {
  bool phrase = false;
  std::uint32_t start = 0;
  std::uint16_t size = 0;
  Phrase phrase_data{};
};

using Plan = std::vector<Segment>;
using StreamRefs = std::vector<const Bytes *>;
using PhraseCounts = std::unordered_map<Phrase, std::size_t, PhraseHash>;
using PhraseSet = std::unordered_set<Phrase, PhraseHash>;
using PhraseCosts = std::unordered_map<Phrase, double, PhraseHash>;

struct Encoding {
  std::vector<Plan> plans;
  std::size_t size = std::numeric_limits<std::size_t>::max();
};

struct Bank {
  std::size_t first_frame = 0;
  std::size_t end_frame = 0;
  Encoding encoding;
  std::size_t size = 0;
};

std::uint8_t phrase_byte(const Phrase &phrase, std::size_t index) {
  return static_cast<std::uint8_t>((phrase.data >> (index * 8)) & 0xff);
}

bool PhraseLess::operator()(const Phrase &left,
                            const Phrase &right) const noexcept {
  const auto common = std::min(left.size, right.size);
  for (std::size_t index = 0; index < common; ++index) {
    if (phrase_byte(left, index) != phrase_byte(right, index)) {
      return phrase_byte(left, index) < phrase_byte(right, index);
    }
  }
  return left.size < right.size;
}

Phrase make_phrase(const Bytes &bytes, std::size_t start, std::size_t size) {
  Phrase result{0, static_cast<std::uint8_t>(size)};
  for (std::size_t index = 0; index < size; ++index) {
    result.data |= static_cast<std::uint64_t>(bytes[start + index])
                   << (index * 8);
  }
  return result;
}

std::size_t definition_cost(const Phrase &phrase) {
  // Four-write phrases naturally stop at the end of the decoder's unrolled
  // loop. Only shorter phrases need a zero-register terminator.
  return phrase.size + (phrase.size < 8 ? 1 : 0);
}

bool valid_phrase(const Phrase &phrase) {
  // The first two writes are unconditional. Registers three and four are
  // checked for zero by the player and therefore cannot themselves be zero.
  return (phrase.size < 6 || phrase_byte(phrase, 4) != 0) &&
         (phrase.size < 8 || phrase_byte(phrase, 6) != 0);
}

class JsonReader {
public:
  explicit JsonReader(std::string input) : input_(std::move(input)) {}

  std::size_t read_number() {
    skip_whitespace();
    if (position_ == input_.size() ||
        !std::isdigit(static_cast<unsigned char>(input_[position_]))) {
      fail("expected an unsigned number");
    }
    std::size_t value = 0;
    while (position_ < input_.size() &&
           std::isdigit(static_cast<unsigned char>(input_[position_]))) {
      value = value * 10 + static_cast<unsigned>(input_[position_] - '0');
      ++position_;
    }
    return value;
  }

  std::vector<Frame> read_frames() {
    std::vector<Frame> frames;
    expect('[');
    if (consume(']')) {
      return frames;
    }
    for (;;) {
      Frame frame;
      expect('[');
      frame.ports[0] = read_byte_array();
      expect(',');
      frame.ports[1] = read_byte_array();
      expect(']');
      frames.push_back(std::move(frame));
      if (consume(']')) {
        break;
      }
      expect(',');
    }
    return frames;
  }

  void require_end() {
    skip_whitespace();
    if (position_ != input_.size()) {
      fail("unexpected trailing data");
    }
  }

private:
  Bytes read_byte_array() {
    Bytes bytes;
    expect('[');
    if (consume(']')) {
      return bytes;
    }
    for (;;) {
      const auto value = read_number();
      if (value > 0xff) {
        fail("byte value is greater than 255");
      }
      bytes.push_back(static_cast<std::uint8_t>(value));
      if (consume(']')) {
        break;
      }
      expect(',');
    }
    return bytes;
  }

  void skip_whitespace() {
    while (position_ < input_.size() &&
           std::isspace(static_cast<unsigned char>(input_[position_]))) {
      ++position_;
    }
  }

  bool consume(char wanted) {
    skip_whitespace();
    if (position_ < input_.size() && input_[position_] == wanted) {
      ++position_;
      return true;
    }
    return false;
  }

  void expect(char wanted) {
    if (!consume(wanted)) {
      fail(std::string("expected '") + wanted + "'");
    }
  }

  [[noreturn]] void fail(const std::string &message) const {
    throw std::runtime_error("JSON input at byte " +
                             std::to_string(position_) + ": " + message);
  }

  std::string input_;
  std::size_t position_ = 0;
};

StreamRefs frame_streams(const std::vector<Frame> &frames,
                         std::size_t first = 0,
                         std::optional<std::size_t> end = std::nullopt) {
  const auto last = end.value_or(frames.size());
  StreamRefs streams;
  streams.reserve((last - first) * 2);
  for (std::size_t index = first; index < last; ++index) {
    streams.push_back(&frames[index].ports[0]);
    streams.push_back(&frames[index].ports[1]);
  }
  return streams;
}

PhraseCounts collect_candidates(const StreamRefs &streams) {
  PhraseCounts counts;
  std::size_t total_writes = 0;
  for (const auto *stream : streams) {
    total_writes += stream->size() / 2;
  }
  counts.reserve(total_writes);

  for (const auto *stream : streams) {
    for (std::size_t start = 0; start < stream->size(); start += 2) {
      for (const auto length : kPhraseLengths) {
        if (start + length > stream->size()) {
          continue;
        }
        const auto phrase = make_phrase(*stream, start, length);
        if (valid_phrase(phrase)) {
          ++counts[phrase];
        }
      }
    }
  }

  for (auto iterator = counts.begin(); iterator != counts.end();) {
    if (iterator->second < 2) {
      iterator = counts.erase(iterator);
    } else {
      ++iterator;
    }
  }
  return counts;
}

Plan parse_stream(const Bytes &stream, const PhraseCosts &phrase_costs) {
  const auto writes = stream.size() / 2;
  if (writes == 0) {
    return {};
  }

  std::vector<double> literal_before(writes + 1, 0.0);
  std::vector<double> nonliteral_before(writes + 1, 0.0);
  std::vector<std::uint8_t> literal_choice(writes, 0);
  std::vector<std::uint8_t> nonliteral_choice(writes, 0);

  for (std::size_t write = writes; write-- > 0;) {
    auto best_literal = 2.0 + literal_before[write + 1];
    auto best_nonliteral = 4.0 + literal_before[write + 1];
    std::uint8_t best_literal_length = 0;
    std::uint8_t best_nonliteral_length = 0;

    for (const auto length : kPhraseLengths) {
      const auto phrase_writes = length / 2;
      if (write + phrase_writes > writes) {
        continue;
      }
      const auto phrase = make_phrase(stream, write * 2, length);
      const auto found = phrase_costs.find(phrase);
      if (found == phrase_costs.end()) {
        continue;
      }
      const auto cost = found->second + nonliteral_before[write + phrase_writes];
      if (cost <= best_literal) {
        best_literal = cost;
        best_literal_length = static_cast<std::uint8_t>(phrase_writes);
      }
      if (cost <= best_nonliteral) {
        best_nonliteral = cost;
        best_nonliteral_length = static_cast<std::uint8_t>(phrase_writes);
      }
    }

    literal_before[write] = best_literal;
    nonliteral_before[write] = best_nonliteral;
    literal_choice[write] = best_literal_length;
    nonliteral_choice[write] = best_nonliteral_length;
  }

  Plan result;
  bool previous_literal = false;
  std::size_t write = 0;
  while (write < writes) {
    const auto phrase_writes = previous_literal ? literal_choice[write]
                                                : nonliteral_choice[write];
    if (phrase_writes != 0) {
      const auto start = write * 2;
      const auto size = static_cast<std::size_t>(phrase_writes) * 2;
      result.push_back(
          {true, static_cast<std::uint32_t>(start),
           static_cast<std::uint16_t>(size), make_phrase(stream, start, size)});
      write += phrase_writes;
      previous_literal = false;
      continue;
    }

    const auto start = write * 2;
    ++write;
    while (write < writes && literal_choice[write] == 0) {
      ++write;
    }
    const auto size = write * 2 - start;
    result.push_back({false, static_cast<std::uint32_t>(start),
                      static_cast<std::uint16_t>(size), {}});
    previous_literal = true;
  }
  return result;
}

std::vector<Plan> parse_streams(const StreamRefs &streams,
                                const PhraseCosts &phrase_costs) {
  std::vector<Plan> plans;
  plans.reserve(streams.size());
  for (const auto *stream : streams) {
    plans.push_back(parse_stream(*stream, phrase_costs));
  }
  return plans;
}

PhraseCounts phrases_used(const std::vector<Plan> &plans) {
  PhraseCounts used;
  for (const auto &plan : plans) {
    for (const auto &segment : plan) {
      if (segment.phrase) {
        ++used[segment.phrase_data];
      }
    }
  }
  return used;
}

std::size_t literal_segment_cost(std::size_t bytes) {
  const auto writes = bytes / 2;
  const auto chunks = (writes + kMaxLiteralWrites - 1) / kMaxLiteralWrites;
  return bytes + 2 * chunks;
}

std::size_t stream_cost(const Plan &plan) {
  if (plan.empty()) {
    return 1;
  }
  std::size_t size = 0;
  for (const auto &segment : plan) {
    size += segment.phrase ? 2 : literal_segment_cost(segment.size);
  }
  return size;
}

std::size_t encoding_size(const std::vector<Plan> &plans) {
  std::size_t size = 0;
  for (const auto &plan : plans) {
    size += stream_cost(plan);
  }
  for (const auto &[phrase, count] : phrases_used(plans)) {
    static_cast<void>(count);
    size += definition_cost(phrase);
  }
  return size;
}

std::unordered_map<Phrase, std::int64_t, PhraseHash>
phrase_removal_costs(const std::vector<Plan> &plans) {
  std::unordered_map<Phrase, std::int64_t, PhraseHash> deltas;
  for (const auto &plan : plans) {
    std::size_t index = 0;
    while (index < plan.size()) {
      if (!plan[index].phrase) {
        ++index;
        continue;
      }

      const auto phrase = plan[index].phrase_data;
      auto end = index + 1;
      while (end < plan.size() && plan[end].phrase &&
             plan[end].phrase_data == phrase) {
        ++end;
      }

      const auto occurrences = static_cast<std::int64_t>(end - index);
      auto literal_neighbours = std::int64_t{0};
      literal_neighbours += index > 0 && !plan[index - 1].phrase ? 1 : 0;
      literal_neighbours += end < plan.size() && !plan[end].phrase ? 1 : 0;
      deltas[phrase] +=
          occurrences * (static_cast<std::int64_t>(phrase.size) - 2) + 2 -
          2 * literal_neighbours;
      index = end;
    }
  }
  return deltas;
}

void keep_better(Encoding &best, std::vector<Plan> plans) {
  const auto size = encoding_size(plans);
  if (size < best.size) {
    best = {std::move(plans), size};
  }
}

Encoding eliminate_unprofitable_phrases(const StreamRefs &streams,
                                        const PhraseSet &seed) {
  PhraseSet available = seed;
  Encoding best;
  keep_better(best, parse_streams(streams, {}));

  while (!available.empty()) {
    PhraseCosts costs;
    costs.reserve(available.size());
    for (const auto &phrase : available) {
      costs.emplace(phrase, 2.0);
    }

    auto plans = parse_streams(streams, costs);
    const auto used = phrases_used(plans);
    const auto removal_costs = phrase_removal_costs(plans);
    keep_better(best, std::move(plans));

    PhraseSet losers;
    for (const auto &[phrase, count] : used) {
      static_cast<void>(count);
      if (removal_costs.at(phrase) <=
          static_cast<std::int64_t>(definition_cost(phrase))) {
        losers.insert(phrase);
      }
    }
    if (losers.empty()) {
      break;
    }
    for (const auto &phrase : losers) {
      available.erase(phrase);
    }
  }
  return best;
}

Encoding reweighted_encoding(const StreamRefs &streams,
                             const PhraseCounts &candidates, double weight) {
  std::unordered_map<Phrase, double, PhraseHash> estimates;
  estimates.reserve(candidates.size());
  for (const auto &[phrase, count] : candidates) {
    estimates.emplace(phrase, static_cast<double>(count));
  }

  Encoding best;
  keep_better(best, parse_streams(streams, {}));
  for (int iteration = 0; iteration < 8 && !estimates.empty(); ++iteration) {
    PhraseCosts costs;
    costs.reserve(estimates.size());
    for (const auto &[phrase, estimate] : estimates) {
      costs.emplace(phrase,
                    2.0 + weight * definition_cost(phrase) / estimate);
    }

    auto plans = parse_streams(streams, costs);
    const auto used = phrases_used(plans);
    keep_better(best, std::move(plans));

    std::unordered_map<Phrase, double, PhraseHash> next_estimates;
    next_estimates.reserve(used.size());
    for (const auto &[phrase, count] : used) {
      next_estimates.emplace(
          phrase, (estimates.at(phrase) + static_cast<double>(count)) / 2.0);
    }
    estimates = std::move(next_estimates);
  }

  PhraseSet selected;
  for (const auto &[phrase, count] : phrases_used(best.plans)) {
    static_cast<void>(count);
    selected.insert(phrase);
  }
  if (!selected.empty()) {
    const auto polished = eliminate_unprofitable_phrases(streams, selected);
    if (polished.size < best.size) {
      best = polished;
    }
  }
  return best;
}

Encoding optimize_streams(const StreamRefs &streams,
                          const std::vector<Plan> *fallback = nullptr) {
  Encoding best;
  keep_better(best, parse_streams(streams, {}));
  if (fallback != nullptr) {
    keep_better(best, *fallback);
  }

  const auto candidates = collect_candidates(streams);
  if (candidates.empty()) {
    return best;
  }

  PhraseSet all_candidates;
  all_candidates.reserve(candidates.size());
  for (const auto &[phrase, count] : candidates) {
    static_cast<void>(count);
    all_candidates.insert(phrase);
  }

  auto variant = eliminate_unprofitable_phrases(streams, all_candidates);
  if (variant.size < best.size) {
    best = std::move(variant);
  }
  for (const auto weight : {0.75, 1.0, 1.25}) {
    variant = reweighted_encoding(streams, candidates, weight);
    if (variant.size < best.size) {
      best = std::move(variant);
    }
  }
  return best;
}

void validate_encoding(const StreamRefs &streams,
                       const std::vector<Plan> &plans) {
  if (streams.size() != plans.size()) {
    throw std::logic_error("encoding changed the number of streams");
  }
  for (std::size_t index = 0; index < streams.size(); ++index) {
    const auto &stream = *streams[index];
    std::size_t position = 0;
    for (const auto &segment : plans[index]) {
      if (segment.start != position || position + segment.size > stream.size()) {
        throw std::logic_error("encoding does not cover its input in order");
      }
      if (segment.phrase) {
        if (segment.phrase_data !=
                make_phrase(stream, segment.start, segment.size) ||
            !valid_phrase(segment.phrase_data)) {
          throw std::logic_error("encoding contains an invalid phrase");
        }
      }
      position += segment.size;
    }
    if (position != stream.size()) {
      throw std::logic_error("encoding does not reproduce its input stream");
    }
  }
}

std::size_t initial_bank_end(const std::vector<Plan> &global_plans,
                             std::size_t start_frame,
                             std::size_t frame_count) {
  auto size = kBankTrailerSize;
  PhraseSet used;
  auto end_frame = start_frame;

  while (end_frame < frame_count) {
    auto extra = std::size_t{0};
    PhraseSet new_phrases;
    for (std::size_t port = 0; port < 2; ++port) {
      const auto &plan = global_plans[end_frame * 2 + port];
      extra += stream_cost(plan);
      for (const auto &segment : plan) {
        if (segment.phrase && !used.contains(segment.phrase_data)) {
          new_phrases.insert(segment.phrase_data);
        }
      }
    }
    for (const auto &phrase : new_phrases) {
      extra += definition_cost(phrase);
    }

    if (end_frame > start_frame && size + extra > kBankSize) {
      break;
    }
    if (size + extra > kBankSize) {
      throw std::runtime_error("frame " + std::to_string(start_frame) +
                               " cannot fit in a music bank");
    }
    size += extra;
    used.insert(new_phrases.begin(), new_phrases.end());
    ++end_frame;
  }
  return end_frame;
}

std::vector<Bank> build_banks(const std::vector<Frame> &frames) {
  const auto all_streams = frame_streams(frames);
  const auto global_encoding = optimize_streams(all_streams);
  validate_encoding(all_streams, global_encoding.plans);

  std::vector<Bank> banks;
  std::size_t start = 0;
  while (start < frames.size()) {
    std::unordered_map<std::size_t, Encoding> cache;

    const auto optimize_range = [&](std::size_t end) -> const Encoding & {
      if (const auto found = cache.find(end); found != cache.end()) {
        return found->second;
      }

      const auto streams = frame_streams(frames, start, end);
      std::vector<Plan> fallback(
          global_encoding.plans.begin() + static_cast<std::ptrdiff_t>(start * 2),
          global_encoding.plans.begin() + static_cast<std::ptrdiff_t>(end * 2));
      auto encoding = optimize_streams(streams, &fallback);
      validate_encoding(streams, encoding.plans);
      return cache.emplace(end, std::move(encoding)).first->second;
    };

    auto end = initial_bank_end(global_encoding.plans, start, frames.size());
    auto best_end = end;
    auto best_encoding = optimize_range(end);
    if (best_encoding.size + kBankTrailerSize > kBankSize) {
      throw std::logic_error("fallback bank encoding unexpectedly grew");
    }

    auto step = std::max<std::size_t>(1, (end - start) / 8);
    std::optional<std::size_t> first_failing_end;
    while (best_end < frames.size()) {
      const auto trial_end = std::min(frames.size(), best_end + step);
      const auto &trial = optimize_range(trial_end);
      if (trial.size + kBankTrailerSize <= kBankSize) {
        best_end = trial_end;
        best_encoding = trial;
        step *= 2;
      } else {
        first_failing_end = trial_end;
        break;
      }
    }

    if (first_failing_end.has_value()) {
      auto low = best_end + 1;
      auto high = *first_failing_end - 1;
      while (low <= high) {
        const auto middle = (low + high) / 2;
        const auto &trial = optimize_range(middle);
        if (trial.size + kBankTrailerSize <= kBankSize) {
          best_end = middle;
          best_encoding = trial;
          low = middle + 1;
        } else {
          high = middle - 1;
        }
      }
    }

    const auto bank_size = best_encoding.size + kBankTrailerSize;
    if (bank_size > kBankSize) {
      throw std::logic_error("music bank overflow");
    }
    banks.push_back({start, best_end, std::move(best_encoding), bank_size});
    start = best_end;
  }
  return banks;
}

std::string hex_byte(std::uint8_t value) {
  std::ostringstream output;
  output << '$' << std::hex << std::setfill('0') << std::setw(2)
         << static_cast<unsigned>(value);
  return output.str();
}

using PhraseLabels = std::unordered_map<Phrase, std::string, PhraseHash>;

void emit_literal(std::ostream &output, const Bytes &stream,
                  const Segment &segment, bool ends_port) {
  const auto end = static_cast<std::size_t>(segment.start) + segment.size;
  for (auto offset = static_cast<std::size_t>(segment.start); offset < end;) {
    const auto chunk_size = std::min(kMaxLiteralWrites * 2, end - offset);
    const auto last = offset + chunk_size == end;
    output << "db " << (ends_port && last ? "f_vgm_literals_end"
                                            : "f_vgm_literals")
           << ", " << chunk_size / 2 << '\n';
    for (auto index = offset; index < offset + chunk_size; index += 2) {
      output << "db " << hex_byte(stream[index]) << ','
             << hex_byte(stream[index + 1]) << '\n';
    }
    offset += chunk_size;
  }
}

void emit_stream(std::ostream &output, const Bytes &stream, const Plan &plan,
                 const PhraseLabels &labels) {
  if (plan.empty()) {
    output << "db f_switch_port\n";
    return;
  }

  for (std::size_t index = 0; index < plan.size(); ++index) {
    const auto &segment = plan[index];
    const auto last = index + 1 == plan.size();
    if (segment.phrase) {
      output << " dwbe " << labels.at(segment.phrase_data)
             << (last ? " | (1<<15)" : "") << '\n';
    } else {
      emit_literal(output, stream, segment, last);
    }
  }
}

void emit_assembly(std::ostream &output, const std::string &song_name,
                   const std::vector<Frame> &frames, std::size_t loop_frame,
                   const std::vector<Bank> &banks) {
  const auto loop_bank = static_cast<std::size_t>(std::distance(
      banks.begin(), std::find_if(banks.begin(), banks.end(), [&](const Bank &bank) {
        return bank.first_frame <= loop_frame && loop_frame < bank.end_frame;
      })));
  if (loop_bank == banks.size()) {
    throw std::logic_error("loop frame is not present in any output bank");
  }

  output << "include \"music_macros.inc\"\n";
  for (std::size_t bank_number = 0; bank_number < banks.size(); ++bank_number) {
    const auto &bank = banks[bank_number];
    std::vector<Phrase> phrases;
    for (const auto &[phrase, count] : phrases_used(bank.encoding.plans)) {
      static_cast<void>(count);
      phrases.push_back(phrase);
    }
    std::sort(phrases.begin(), phrases.end(), PhraseLess{});

    PhraseLabels labels;
    labels.reserve(phrases.size());
    for (std::size_t index = 0; index < phrases.size(); ++index) {
      labels.emplace(phrases[index], song_name + "_phrase_" +
                                         std::to_string(bank_number) + "_" +
                                         std::to_string(index));
    }

    output << "SECTION \"music__" << song_name << bank_number
           << "\", ROMX[$4000]\n";
    output << song_name << bank_number << "::\n";

    for (auto frame_number = bank.first_frame; frame_number < bank.end_frame;
         ++frame_number) {
      if (frame_number == loop_frame) {
        output << song_name << "_loop:\n";
      }
      output << "; BEGIN FRAME\n";
      const auto local_frame = frame_number - bank.first_frame;
      for (std::size_t port = 0; port < 2; ++port) {
        emit_stream(output, frames[frame_number].ports[port],
                    bank.encoding.plans[local_frame * 2 + port], labels);
      }
    }

    std::size_t destination_bank;
    std::string destination;
    if (bank_number + 1 < banks.size()) {
      destination_bank = bank_number + 1;
      destination = song_name + std::to_string(destination_bank);
    } else {
      destination_bank = loop_bank;
      destination = song_name + "_loop";
    }
    output << "db f_switch_bank, LOW(BANK(" << song_name << destination_bank
           << "))\n";
    output << "dw " << destination << '\n';

    for (const auto &phrase : phrases) {
      output << labels.at(phrase) << ": db ";
      for (std::size_t index = 0; index < phrase.size; ++index) {
        if (index != 0) {
          output << ',';
        }
        output << hex_byte(phrase_byte(phrase, index));
      }
      if (phrase.size < 8) {
        output << ",0";
      }
      output << '\n';
    }
  }
}

} // namespace

int main(int argc, char **argv) {
  try {
    if (argc != 2) {
      std::cerr << "usage: " << argv[0] << " SONG_NAME\n";
      return 2;
    }

    std::ostringstream input_buffer;
    input_buffer << std::cin.rdbuf();
    JsonReader reader(input_buffer.str());
    const auto loop_frame = reader.read_number();
    const auto frames = reader.read_frames();
    reader.require_end();

    if (frames.empty()) {
      throw std::runtime_error("cooked song contains no frames");
    }
    if (loop_frame >= frames.size()) {
      throw std::runtime_error("loop frame is outside the cooked song");
    }
    for (const auto &frame : frames) {
      for (const auto &port : frame.ports) {
        if (port.size() % 2 != 0) {
          throw std::runtime_error(
              "register/value data contains an odd number of bytes");
        }
      }
    }

    const auto song_name = std::filesystem::path(argv[1]).filename().string();
    const auto banks = build_banks(frames);
    emit_assembly(std::cout, song_name, frames, loop_frame, banks);

    std::size_t total_size = 0;
    for (const auto &bank : banks) {
      total_size += bank.size;
    }
    std::cerr << "emitted: " << total_size << '\n';
    std::cerr << "banks: " << banks.size() << '\n';
    return 0;
  } catch (const std::exception &error) {
    std::cerr << "vgmcompressor4: " << error.what() << '\n';
    return 1;
  }
}
