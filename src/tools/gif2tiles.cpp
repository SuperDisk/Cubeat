#include <gif_lib.h>

#include <algorithm>
#include <array>
#include <compare>
#include <cstdint>
#include <cstdlib>
#include <filesystem>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <limits>
#include <map>
#include <memory>
#include <numeric>
#include <optional>
#include <sstream>
#include <stdexcept>
#include <string>
#include <tuple>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <vector>

namespace {

constexpr int kScreenWidth = 160;
constexpr int kScreenHeight = 144;
constexpr int kMapWidth = 20;
constexpr int kMapHeight = 18;
constexpr int kTileRows = 8;
constexpr int kNormalReservedTiles = 8;

using Tile = std::array<std::uint16_t, kTileRows>;

struct TileHash {
  std::size_t operator()(const Tile &tile) const noexcept {
    std::size_t hash = 0xcbf29ce484222325ULL;
    for (const auto row : tile) {
      hash ^= row;
      hash *= 0x100000001b3ULL;
    }
    return hash;
  }
};

struct Options {
  std::filesystem::path input;
  std::filesystem::path output;
  bool menu_mode = false;
  bool no_colon = false;
};

struct Frame {
  std::vector<int> tiles;
  std::vector<int> unique_tiles;
};

struct Animation {
  std::vector<Tile> tiles;
  std::vector<Frame> frames;
};

struct WriteGroups {
  std::map<std::uint16_t, std::vector<std::uint16_t>> top;
  std::map<std::uint16_t, std::vector<std::uint16_t>> bottom;
  std::optional<std::uint16_t> shared_boundary;
  int stores = 0;
  int loads = 0;
  int cycles = 0;
};

struct FrameMetrics {
  int replacements = 0;
  int row_stores = 0;
  int row_loads = 0;
  int gfx_cycles = 0;
};

struct Plan {
  std::vector<std::vector<int>> assignments;
  std::vector<std::vector<int>> tilemaps;
  std::vector<FrameMetrics> metrics;
};

std::string hex_value(std::uint32_t value) {
  std::ostringstream stream;
  stream << '$' << std::uppercase << std::hex << value;
  return stream.str();
}

[[noreturn]] void usage(const char *program, const std::string &error = {}) {
  if (!error.empty()) {
    std::cerr << "error: " << error << "\n\n";
  }
  std::cerr << "usage: " << program
            << " INPUT.deop.gif OUTPUT.asm [--menu] [--no-colon]\n";
  std::exit(2);
}

Options parse_options(int argc, char **argv) {
  if (argc < 3) {
    usage(argv[0]);
  }

  Options options{argv[1], argv[2]};
  for (int index = 3; index < argc; ++index) {
    const std::string argument = argv[index];
    if (argument == "--menu" || argument == "-m") {
      options.menu_mode = true;
    } else if (argument == "--no-colon" || argument == "-n") {
      options.no_colon = true;
    } else {
      usage(argv[0], "unknown option: " + argument);
    }
  }
  if (options.menu_mode && options.no_colon) {
    usage(argv[0], "--menu and --no-colon cannot be combined");
  }
  return options;
}

int rounded_frame_count(int delay_centiseconds) {
  const int milliseconds = delay_centiseconds * 10;
  int quotient = milliseconds / 32;
  const int remainder = milliseconds % 32;
  if (remainder * 2 > 32 ||
      (remainder * 2 == 32 && (quotient & 1) != 0)) {
    ++quotient;
  }
  return quotient;
}

std::vector<std::vector<std::uint8_t>> load_gif_frames(
    const std::filesystem::path &filename, bool add_colon) {
  int error = 0;
  GifFileType *raw_gif = DGifOpenFileName(filename.c_str(), &error);
  if (raw_gif == nullptr) {
    throw std::runtime_error("could not open GIF " + filename.string() +
                             ": " + GifErrorString(error));
  }

  struct GifCloser {
    void operator()(GifFileType *gif) const {
      int close_error = 0;
      DGifCloseFile(gif, &close_error);
    }
  };
  const std::unique_ptr<GifFileType, GifCloser> gif(raw_gif);

  if (DGifSlurp(gif.get()) == GIF_ERROR) {
    throw std::runtime_error("could not decode GIF " + filename.string() +
                             ": " + GifErrorString(gif->Error));
  }
  if (gif->SWidth != kScreenWidth || gif->SHeight != kScreenHeight) {
    throw std::runtime_error("GIF must be exactly 160x144 pixels");
  }

  std::vector<std::vector<std::uint8_t>> frames;
  for (int image_index = 0; image_index < gif->ImageCount; ++image_index) {
    const SavedImage &image = gif->SavedImages[image_index];
    if (image.ImageDesc.Left != 0 || image.ImageDesc.Top != 0 ||
        image.ImageDesc.Width != kScreenWidth ||
        image.ImageDesc.Height != kScreenHeight) {
      throw std::runtime_error(
          "GIF frames must be unoptimized full 160x144 images; use the "
          "Makefile .deop.gif rule");
    }

    GraphicsControlBlock control{};
    if (DGifSavedExtensionToGCB(gif.get(), image_index, &control) == GIF_ERROR) {
      throw std::runtime_error("could not read GIF frame timing");
    }

    std::vector<std::uint8_t> pixels(image.RasterBits,
                                     image.RasterBits + kScreenWidth *
                                                            kScreenHeight);
    if (std::any_of(pixels.begin(), pixels.end(),
                    [](std::uint8_t pixel) { return pixel > 3; })) {
      throw std::runtime_error(
          "GIF contains palette indexes above 3; apply background.colormap");
    }

    if (add_colon) {
      constexpr std::array<std::uint8_t, 8> colon = {3, 3, 0, 0,
                                                     3, 3, 0, 0};
      for (int y = 0; y < 4; ++y) {
        for (int x = 0; x < 2; ++x) {
          pixels[(11 + y) * kScreenWidth + 80 + x] = colon[y * 2 + x];
        }
      }
    }

    const int repetitions = rounded_frame_count(control.DelayTime);
    for (int repetition = 0; repetition < repetitions; ++repetition) {
      frames.push_back(pixels);
    }
  }

  if (frames.empty()) {
    throw std::runtime_error("GIF produced no 32 ms animation frames");
  }
  return frames;
}

Tile make_tile(const std::vector<std::uint8_t> &pixels, int tile_x,
               int tile_y) {
  Tile tile{};
  for (int y = 0; y < 8; ++y) {
    std::uint8_t high_plane = 0;
    std::uint8_t low_plane = 0;
    for (int x = 0; x < 8; ++x) {
      const auto pixel = pixels[(tile_y * 8 + y) * kScreenWidth +
                                tile_x * 8 + x];
      const int bit = 7 - x;
      high_plane |= static_cast<std::uint8_t>(((pixel >> 1) & 1) << bit);
      low_plane |= static_cast<std::uint8_t>((pixel & 1) << bit);
    }
    tile[y] = static_cast<std::uint16_t>(high_plane) |
              (static_cast<std::uint16_t>(low_plane) << 8);
  }
  return tile;
}

Animation make_animation(
    const std::vector<std::vector<std::uint8_t>> &pixel_frames) {
  Animation animation;
  std::unordered_map<Tile, int, TileHash> tile_ids;

  for (const auto &pixels : pixel_frames) {
    Frame frame;
    frame.tiles.reserve(kMapWidth * kMapHeight);
    std::unordered_set<int> unique;
    for (int tile_y = 0; tile_y < kMapHeight; ++tile_y) {
      for (int tile_x = 0; tile_x < kMapWidth; ++tile_x) {
        const Tile tile = make_tile(pixels, tile_x, tile_y);
        const auto [entry, inserted] =
            tile_ids.emplace(tile, static_cast<int>(animation.tiles.size()));
        if (inserted) {
          animation.tiles.push_back(tile);
        }
        frame.tiles.push_back(entry->second);
        if (unique.insert(entry->second).second) {
          frame.unique_tiles.push_back(entry->second);
        }
      }
    }
    animation.frames.push_back(std::move(frame));
  }
  return animation;
}

int differing_rows(const Tile &before, const Tile &after) {
  int count = 0;
  for (int row = 0; row < kTileRows; ++row) {
    count += before[row] != after[row];
  }
  return count;
}

std::vector<int> hungarian(const std::vector<std::vector<long long>> &cost) {
  const int rows = static_cast<int>(cost.size());
  const int columns = rows == 0 ? 0 : static_cast<int>(cost.front().size());
  if (rows > columns) {
    throw std::runtime_error("internal assignment matrix is not rectangular");
  }
  if (rows == 0) {
    return {};
  }

  constexpr long long infinity = std::numeric_limits<long long>::max() / 4;
  std::vector<long long> u(rows + 1), v(columns + 1);
  std::vector<int> p(columns + 1), way(columns + 1);
  for (int row = 1; row <= rows; ++row) {
    p[0] = row;
    int column0 = 0;
    std::vector<long long> min_value(columns + 1, infinity);
    std::vector<bool> used(columns + 1, false);
    do {
      used[column0] = true;
      const int row0 = p[column0];
      long long delta = infinity;
      int column1 = 0;
      for (int column = 1; column <= columns; ++column) {
        if (used[column]) {
          continue;
        }
        const long long current =
            cost[row0 - 1][column - 1] - u[row0] - v[column];
        if (current < min_value[column]) {
          min_value[column] = current;
          way[column] = column0;
        }
        if (min_value[column] < delta) {
          delta = min_value[column];
          column1 = column;
        }
      }
      for (int column = 0; column <= columns; ++column) {
        if (used[column]) {
          u[p[column]] += delta;
          v[column] -= delta;
        } else {
          min_value[column] -= delta;
        }
      }
      column0 = column1;
    } while (p[column0] != 0);

    do {
      const int column1 = way[column0];
      p[column0] = p[column1];
      column0 = column1;
    } while (column0 != 0);
  }

  std::vector<int> result(rows, -1);
  for (int column = 1; column <= columns; ++column) {
    if (p[column] != 0) {
      result[p[column] - 1] = column - 1;
    }
  }
  return result;
}

class ResidencyPlanner {
 public:
  ResidencyPlanner(const Animation &animation, int reserved_tiles)
      : animation_(animation),
        reserved_tiles_(reserved_tiles),
        capacity_(256 - reserved_tiles),
        required_(animation.frames.size(),
                  std::vector<std::uint8_t>(animation.tiles.size(), 0)),
        next_use_(animation.frames.size(),
                  std::vector<int>(animation.tiles.size(), 0)) {
    for (std::size_t frame = 0; frame < animation_.frames.size(); ++frame) {
      if (static_cast<int>(animation_.frames[frame].unique_tiles.size()) >
          capacity_) {
        throw std::runtime_error(
            "frame " + std::to_string(frame) + " needs " +
            std::to_string(animation_.frames[frame].unique_tiles.size()) +
            " unique tiles, but only " + std::to_string(capacity_) +
            " background slots are available");
      }
      for (const int tile : animation_.frames[frame].unique_tiles) {
        required_[frame][tile] = 1;
      }
    }
    build_next_use();
  }

  Plan make_plan(bool menu_mode) {
    if (menu_mode) {
      if (static_cast<int>(animation_.tiles.size()) > capacity_) {
        throw std::runtime_error(
            "menu animation has " + std::to_string(animation_.tiles.size()) +
            " unique tiles, but menu mode cannot upload graphics after init");
      }
      Plan plan;
      const std::vector<int> assignment(animation_.tiles.size(), 0);
      std::vector<int> populated = assignment;
      std::iota(populated.begin(), populated.end(), 0);
      plan.assignments.assign(animation_.frames.size(), populated);
      finish_plan(plan);
      return plan;
    }

    // Plan cache contents over the whole repeating animation. Farthest-next-use
    // eviction minimizes misses, but its slot layout is scored separately below.
    auto content = initial_state();
    std::sort(content.begin(), content.end());
    std::vector<std::vector<int>> best_contents;
    auto best_score = std::tuple{std::numeric_limits<int>::max(),
                                 std::numeric_limits<long long>::max(),
                                 std::numeric_limits<long long>::max()};
    for (int iteration = 0; iteration < 64; ++iteration) {
      const auto start = content;
      std::vector<std::vector<int>> contents(animation_.frames.size());
      contents[0] = content;
      for (std::size_t frame = 1; frame < animation_.frames.size(); ++frame) {
        service_content(static_cast<int>(frame), content);
        contents[frame] = content;
      }

      int maximum_misses = 0;
      long long squares = 0;
      long long total = 0;
      for (int frame = 0; frame < static_cast<int>(contents.size());
           ++frame) {
        const int previous =
            (frame + static_cast<int>(contents.size()) - 1) % contents.size();
        int misses = 0;
        for (const int tile : contents[frame]) {
          misses += !std::binary_search(contents[previous].begin(),
                                        contents[previous].end(), tile);
        }
        maximum_misses = std::max(maximum_misses, misses);
        squares += static_cast<long long>(misses) * misses;
        total += misses;
      }
      const auto score = std::tuple{maximum_misses, squares, total};
      if (score < best_score) {
        best_score = score;
        best_contents = contents;
      }

      service_content(0, content);
      if (content == start) {
        break;
      }
    }
    Plan plan;
    auto offline_assignments = assign_slots(best_contents);

    // The Lisp allocator is a useful peak-cost fallback: its cache contents are
    // not globally optimal, but they naturally close after one animation loop.
    // Re-color those same contents as a second, usually much cheaper candidate.
    auto fifo = fifo_assignments();
    std::vector<std::vector<int>> fifo_contents = fifo;
    for (auto &frame : fifo_contents) {
      std::sort(frame.begin(), frame.end());
    }
    auto remapped_fifo = assign_slots(fifo_contents);
    const auto fifo_score = assignment_score(fifo);
    const auto remapped_fifo_score = assignment_score(remapped_fifo);
    const auto offline_score = assignment_score(offline_assignments);
    plan.assignments = std::move(fifo);
    auto selected_score = fifo_score;
    if (remapped_fifo_score < selected_score) {
      plan.assignments = std::move(remapped_fifo);
      selected_score = remapped_fifo_score;
    }
    if (offline_score < selected_score) {
      plan.assignments = std::move(offline_assignments);
    }
    finish_plan(plan);
    for (int pass = 0; pass < 3; ++pass) {
      smooth_prefetches(plan);
    }
    finish_plan(plan);
    return plan;
  }

 private:
  const Animation &animation_;
  int reserved_tiles_;
  int capacity_;
  std::vector<std::vector<std::uint8_t>> required_;
  std::vector<std::vector<int>> next_use_;

  void build_next_use() {
    const int frame_count = static_cast<int>(animation_.frames.size());
    for (int frame = 0; frame < frame_count; ++frame) {
      for (int tile = 0; tile < static_cast<int>(animation_.tiles.size());
           ++tile) {
        int distance = frame_count + 1;
        for (int step = 1; step <= frame_count; ++step) {
          if (required_[(frame + step) % frame_count][tile]) {
            distance = step;
            break;
          }
        }
        next_use_[frame][tile] = distance;
      }
    }
  }

  std::vector<int> initial_state() const {
    const int slots =
        std::min(capacity_, static_cast<int>(animation_.tiles.size()));
    std::vector<int> state = animation_.frames[0].unique_tiles;
    std::vector<int> extras;
    for (int tile = 0; tile < static_cast<int>(animation_.tiles.size());
         ++tile) {
      if (!required_[0][tile]) {
        extras.push_back(tile);
      }
    }
    std::stable_sort(extras.begin(), extras.end(), [&](int left, int right) {
      if (next_use_[0][left] != next_use_[0][right]) {
        return next_use_[0][left] < next_use_[0][right];
      }
      return left < right;
    });
    for (const int tile : extras) {
      if (static_cast<int>(state.size()) == slots) {
        break;
      }
      state.push_back(tile);
    }
    return state;
  }

  void service_content(int frame, std::vector<int> &content) const {
    std::vector<bool> resident(animation_.tiles.size(), false);
    for (const int tile : content) {
      resident[tile] = true;
    }

    std::vector<int> missing;
    for (const int tile : animation_.frames[frame].unique_tiles) {
      if (!resident[tile]) {
        missing.push_back(tile);
      }
    }
    if (missing.empty()) {
      return;
    }

    std::vector<int> candidates;
    for (const int tile : content) {
      if (!required_[frame][tile]) {
        candidates.push_back(tile);
      }
    }
    if (missing.size() > candidates.size()) {
      throw std::runtime_error("internal residency capacity failure");
    }
    std::sort(candidates.begin(), candidates.end(), [&](int left, int right) {
      if (next_use_[frame][left] != next_use_[frame][right]) {
        return next_use_[frame][left] > next_use_[frame][right];
      }
      return left > right;
    });
    candidates.resize(missing.size());

    for (const int tile : candidates) {
      const auto found = std::lower_bound(content.begin(), content.end(), tile);
      content.erase(found);
    }
    content.insert(content.end(), missing.begin(), missing.end());
    std::sort(content.begin(), content.end());
  }

  std::vector<std::vector<int>> assign_slots(
      const std::vector<std::vector<int>> &contents) const {
    const int frame_count = static_cast<int>(contents.size());
    const int slot_count = static_cast<int>(contents.front().size());

    const auto transition = [&](const std::vector<int> &state,
                                const std::vector<int> &wanted_tiles) {
      std::vector<bool> wanted(animation_.tiles.size(), false);
      for (const int tile : wanted_tiles) {
        wanted[tile] = true;
      }

      std::vector<int> next_state(slot_count, -1);
      std::vector<int> free_slots;
      std::vector<bool> retained(animation_.tiles.size(), false);
      for (int slot = 0; slot < slot_count; ++slot) {
        if (wanted[state[slot]]) {
          next_state[slot] = state[slot];
          retained[state[slot]] = true;
        } else {
          free_slots.push_back(slot);
        }
      }

      std::vector<int> added;
      for (const int tile : wanted_tiles) {
        if (!retained[tile]) {
          added.push_back(tile);
        }
      }
      if (free_slots.size() != added.size()) {
        throw std::runtime_error("slot assignment imbalance");
      }

      if (!added.empty()) {
        std::vector<std::vector<long long>> costs(
            free_slots.size(), std::vector<long long>(added.size()));
        for (std::size_t old_index = 0; old_index < free_slots.size();
             ++old_index) {
          for (std::size_t new_index = 0; new_index < added.size();
               ++new_index) {
            const int old_tile = state[free_slots[old_index]];
            costs[old_index][new_index] =
                differing_rows(animation_.tiles[old_tile],
                               animation_.tiles[added[new_index]]) *
                    1024LL +
                added[new_index];
          }
        }
        const auto matching = hungarian(costs);
        for (std::size_t old_index = 0; old_index < free_slots.size();
             ++old_index) {
          next_state[free_slots[old_index]] = added[matching[old_index]];
        }
      }
      return next_state;
    };

    const auto build_cycle = [&](int cut, std::vector<int> state) {
      std::vector<std::vector<int>> assignments(frame_count);
      assignments[cut] = state;
      for (int step = 1; step < frame_count; ++step) {
        const int frame = (cut + step) % frame_count;
        state = transition(state, contents[frame]);
        assignments[frame] = state;
      }
      return assignments;
    };

    std::vector<std::vector<int>> best_assignments;
    auto best_score = std::tuple{std::numeric_limits<int>::max(),
                                 std::numeric_limits<long long>::max(),
                                 std::numeric_limits<long long>::max()};
    int best_cut = 0;

    // Physical slot choices affect both row similarity and the loop-closing
    // transition. Try every possible cut, then refine the best cyclic mapping.
    for (int cut = 0; cut < frame_count; ++cut) {
      auto assignments = build_cycle(cut, contents[cut]);
      const auto score = assignment_score(assignments);
      if (score < best_score) {
        best_score = score;
        best_cut = cut;
        best_assignments = std::move(assignments);
      }
    }

    std::vector<int> start = contents[best_cut];
    for (int iteration = 0; iteration < 64; ++iteration) {
      auto assignments = build_cycle(best_cut, start);
      const auto score = assignment_score(assignments);
      if (score < best_score) {
        best_score = score;
        best_assignments = assignments;
      }
      const int last = (best_cut + frame_count - 1) % frame_count;
      auto next_start = transition(assignments[last], contents[best_cut]);
      if (next_start == start) {
        break;
      }
      start = std::move(next_start);
    }
    return best_assignments;
  }

  std::vector<std::vector<int>> fifo_assignments() const {
    const int slot_count =
        std::min(capacity_, static_cast<int>(animation_.tiles.size()));
    std::vector<int> initial(slot_count);
    std::iota(initial.begin(), initial.end(), 0);
    return fifo_assignments_from(std::move(initial));
  }

  std::vector<std::vector<int>> fifo_assignments_from(
      std::vector<int> state) const {
    const int frame_count = static_cast<int>(animation_.frames.size());
    const int slot_count = static_cast<int>(state.size());
    std::vector<std::vector<int>> assignments(frame_count);
    assignments[0] = state;

    for (int frame = 1; frame < frame_count; ++frame) {
      // This intentionally reproduces REMOVE-DUPLICATES in the Lisp allocator:
      // inactive slots are effectively reconsidered in ascending slot order.
      std::vector<int> free_queue;
      for (int slot = 0; slot < slot_count; ++slot) {
        if (!required_[frame][state[slot]]) {
          free_queue.push_back(slot);
        }
      }

      std::vector<int> slot_for_tile(animation_.tiles.size(), -1);
      for (int slot = 0; slot < slot_count; ++slot) {
        slot_for_tile[state[slot]] = slot;
      }

      std::vector<int> missing;
      for (const int tile : animation_.frames[frame].unique_tiles) {
        if (slot_for_tile[tile] < 0) {
          missing.push_back(tile);
        }
      }
      if (missing.size() > free_queue.size()) {
        throw std::runtime_error("FIFO residency capacity failure");
      }

      std::vector<int> victims(free_queue.begin(),
                               free_queue.begin() + missing.size());

      for (std::size_t index = 0; index < missing.size(); ++index) {
        state[victims[index]] = missing[index];
      }
      assignments[frame] = state;
    }
    return assignments;
  }

  std::tuple<int, long long, long long> assignment_score(
      const std::vector<std::vector<int>> &assignments) const {
    int maximum = 0;
    long long squares = 0;
    long long total = 0;
    for (int frame = 0; frame < static_cast<int>(assignments.size()); ++frame) {
      const int value = frame_gfx_cycles(frame, assignments);
      maximum = std::max(maximum, value);
      squares += static_cast<long long>(value) * value;
      total += value;
    }
    return {maximum, squares, total};
  }

  std::vector<int> make_tilemap(int frame,
                                const std::vector<int> &assignment) const {
    std::vector<int> slot_for_tile(animation_.tiles.size(), -1);
    for (int slot = 0; slot < static_cast<int>(assignment.size()); ++slot) {
      slot_for_tile[assignment[slot]] = reserved_tiles_ + slot;
    }
    std::vector<int> tilemap;
    tilemap.reserve(kMapWidth * kMapHeight);
    for (const int tile : animation_.frames[frame].tiles) {
      if (slot_for_tile[tile] < 0) {
        throw std::runtime_error("planned frame references a nonresident tile");
      }
      tilemap.push_back(slot_for_tile[tile]);
    }
    return tilemap;
  }

  WriteGroups collect_writes(int /* frame */, const std::vector<int> &before,
                             const std::vector<int> &after,
                             const std::vector<int> &tilemap) const {
    WriteGroups writes;
    std::vector<bool> top_slot(256, false);
    for (int location = 0; location <= 150; ++location) {
      top_slot[tilemap[location]] = true;
    }

    for (int slot = 0; slot < static_cast<int>(after.size()); ++slot) {
      if (before[slot] == after[slot]) {
        continue;
      }
      const int tile_index = reserved_tiles_ + slot;
      const auto &old_tile = animation_.tiles[before[slot]];
      const auto &new_tile = animation_.tiles[after[slot]];
      auto &groups = top_slot[tile_index] ? writes.top : writes.bottom;
      for (int row = 0; row < kTileRows; ++row) {
        // VRAM already contains old_tile. A replacement only needs the rows
        // whose final 16-bit value actually changes.
        if (old_tile[row] == new_tile[row]) {
          continue;
        }
        const auto address = static_cast<std::uint16_t>(
            0x8800 + tile_index * 16 + row * 2);
        groups[new_tile[row]].push_back(address);
        ++writes.stores;
      }
    }

    for (const auto &[word, unused] : writes.top) {
      if (writes.bottom.contains(word)) {
        writes.shared_boundary = word;
        break;
      }
    }
    writes.loads = static_cast<int>(writes.top.size() + writes.bottom.size()) -
                   (writes.shared_boundary.has_value() ? 1 : 0);
    writes.cycles = writes.loads * 3 + writes.stores * 5;
    return writes;
  }

  int frame_gfx_cycles(int frame,
                       const std::vector<std::vector<int>> &assignments) const {
    const int previous =
        (frame + static_cast<int>(assignments.size()) - 1) % assignments.size();
    const auto tilemap = make_tilemap(frame, assignments[frame]);
    return collect_writes(frame, assignments[previous], assignments[frame],
                          tilemap)
        .cycles;
  }

  void finish_plan(Plan &plan) const {
    const int frame_count = static_cast<int>(animation_.frames.size());
    plan.tilemaps.resize(frame_count);
    plan.metrics.assign(frame_count, {});
    for (int frame = 0; frame < frame_count; ++frame) {
      plan.tilemaps[frame] = make_tilemap(frame, plan.assignments[frame]);
      const int previous = (frame + frame_count - 1) % frame_count;
      const auto writes = collect_writes(frame, plan.assignments[previous],
                                         plan.assignments[frame],
                                         plan.tilemaps[frame]);
      auto &metrics = plan.metrics[frame];
      metrics.replacements = 0;
      for (std::size_t slot = 0; slot < plan.assignments[frame].size(); ++slot) {
        metrics.replacements += plan.assignments[previous][slot] !=
                                plan.assignments[frame][slot];
      }
      metrics.row_stores = writes.stores;
      metrics.row_loads = writes.loads;
      metrics.gfx_cycles = writes.cycles;
    }
  }

  struct Objective {
    int maximum = 0;
    long long squares = 0;
    long long total = 0;

    auto operator<=>(const Objective &) const = default;
  };

  Objective objective(const std::vector<int> &cycles) const {
    Objective result;
    for (const int cycle : cycles) {
      result.maximum = std::max(result.maximum, cycle);
      result.squares += static_cast<long long>(cycle) * cycle;
      result.total += cycle;
    }
    return result;
  }

  void smooth_prefetches(Plan &plan) const {
    const int frame_count = static_cast<int>(plan.assignments.size());
    if (frame_count < 2) {
      return;
    }
    const int slot_count = static_cast<int>(plan.assignments[0].size());

    struct Event {
      int frame;
      int slot;
      int old_tile;
      int new_tile;
      int original_cost;
    };
    std::vector<Event> events;
    for (int frame = 0; frame < frame_count; ++frame) {
      const int previous = (frame + frame_count - 1) % frame_count;
      for (int slot = 0; slot < slot_count; ++slot) {
        if (plan.assignments[previous][slot] !=
            plan.assignments[frame][slot]) {
          const int old_tile = plan.assignments[previous][slot];
          const int new_tile = plan.assignments[frame][slot];
          events.push_back({frame, slot, old_tile, new_tile,
                            differing_rows(animation_.tiles[old_tile],
                                           animation_.tiles[new_tile])});
        }
      }
    }
    std::stable_sort(events.begin(), events.end(),
                     [](const Event &left, const Event &right) {
                       return left.original_cost > right.original_cost;
                     });

    std::vector<int> cycles(frame_count);
    for (int frame = 0; frame < frame_count; ++frame) {
      cycles[frame] = frame_gfx_cycles(frame, plan.assignments);
    }
    auto current_objective = objective(cycles);

    // A replacement can move earlier through frames where neither endpoint is
    // active. Greedily use that slack to minimize the peak, then variance.
    for (const auto &event : events) {
      const int frame = event.frame;
      const int previous = (frame + frame_count - 1) % frame_count;
      if (plan.assignments[previous][event.slot] != event.old_tile ||
          plan.assignments[frame][event.slot] != event.new_tile) {
        continue;
      }

      std::vector<int> candidates;
      for (int step = 1; step < frame_count; ++step) {
        const int candidate = (frame - step + frame_count) % frame_count;
        if (plan.assignments[candidate][event.slot] != event.old_tile ||
            required_[candidate][event.old_tile] ||
            required_[candidate][event.new_tile]) {
          break;
        }
        bool duplicate = false;
        for (int slot = 0; slot < slot_count; ++slot) {
          if (slot != event.slot &&
              plan.assignments[candidate][slot] == event.new_tile) {
            duplicate = true;
            break;
          }
        }
        if (duplicate) {
          break;
        }
        candidates.push_back(candidate);
      }
      if (candidates.empty()) {
        continue;
      }

      int best_candidate = -1;
      Objective best_objective = current_objective;
      int best_candidate_cycles = 0;
      int best_frame_cycles = 0;

      for (const int candidate : candidates) {
        std::vector<int> changed_frames;
        for (int cursor = candidate; cursor != frame;
             cursor = (cursor + 1) % frame_count) {
          changed_frames.push_back(cursor);
          plan.assignments[cursor][event.slot] = event.new_tile;
        }

        const int candidate_cycles =
            frame_gfx_cycles(candidate, plan.assignments);
        const int frame_cycles = frame_gfx_cycles(frame, plan.assignments);
        auto trial_cycles = cycles;
        trial_cycles[candidate] = candidate_cycles;
        trial_cycles[frame] = frame_cycles;
        const auto trial_objective = objective(trial_cycles);

        for (const int changed : changed_frames) {
          plan.assignments[changed][event.slot] = event.old_tile;
        }

        if (trial_objective < best_objective) {
          best_objective = trial_objective;
          best_candidate = candidate;
          best_candidate_cycles = candidate_cycles;
          best_frame_cycles = frame_cycles;
        }
      }

      if (best_candidate >= 0) {
        for (int cursor = best_candidate; cursor != frame;
             cursor = (cursor + 1) % frame_count) {
          plan.assignments[cursor][event.slot] = event.new_tile;
        }
        cycles[best_candidate] = best_candidate_cycles;
        cycles[frame] = best_frame_cycles;
        current_objective = best_objective;
      }
    }
  }
};

std::vector<int> playfield_buffer_offsets() {
  std::vector<int> offsets;
  offsets.reserve(kMapWidth * kMapHeight);
  int bytes_generated = 0;
  int total_cycles = 0;
  int cycle_counter = 0;

  const auto add_cycles = [&](int amount) {
    total_cycles += amount;
    if (total_cycles > 1090) {
      cycle_counter += amount;
    }
    if (cycle_counter > 42) {
      bytes_generated += 6;
      cycle_counter = 1;
    }
  };

  int row_start = 0;
  for (int y = 0; y < kMapHeight; ++y) {
    for (int x = 0; x < kMapWidth - 1; ++x) {
      offsets.push_back(bytes_generated + 1);
      add_cycles(2);
      add_cycles(2);
      bytes_generated += 3;
    }
    offsets.push_back(bytes_generated + 1);
    add_cycles(3);
    bytes_generated += 2;

    row_start += 0x20;
    add_cycles(2);
    bytes_generated += 2;
    if ((row_start & 0xff) == 0) {
      add_cycles(1);
      ++bytes_generated;
    }
  }
  return offsets;
}

class AssemblyWriter {
 public:
  AssemblyWriter(const Animation &animation, const Plan &plan,
                 const Options &options)
      : animation_(animation),
        plan_(plan),
        options_(options),
        reserved_tiles_(options.menu_mode ? 0 : kNormalReservedTiles),
        prefix_(options.output.stem().stem().string()),
        offsets_(playfield_buffer_offsets()),
        output_(options.output) {
    if (!output_) {
      throw std::runtime_error("could not open output " +
                               options.output.string());
    }
  }

  void write() {
    section("gfx_init");
    output_ << prefix_ << "_gfx_init:\n";
    write_gfx_init();

    if (options_.menu_mode) {
      section("map_init");
      output_ << prefix_ << "_map_init:\n";
      std::vector<int> locations(kMapWidth * kMapHeight);
      std::iota(locations.begin(), locations.end(), 0);
      write_menu_map_diff(0, locations, 1);
    }

    for (int frame = 0; frame < static_cast<int>(animation_.frames.size());
         ++frame) {
      const int next = (frame + 1) % animation_.frames.size();
      if (!options_.menu_mode) {
        section("gfx_" + std::to_string(frame));
        output_ << prefix_ << "_gfx" << frame << ":\n";
        write_gfx_frame(frame, next);
      }

      section("map_" + std::to_string(frame));
      output_ << prefix_ << "_map" << frame << ":\n";
      if (options_.menu_mode) {
        const int previous =
            (frame + static_cast<int>(plan_.tilemaps.size()) - 1) %
            plan_.tilemaps.size();
        std::vector<int> locations;
        for (int location = 0; location < kMapWidth * kMapHeight;
             ++location) {
          if (plan_.tilemaps[previous][location] !=
              plan_.tilemaps[frame][location]) {
            locations.push_back(location);
          }
        }
        write_menu_map_diff(frame, locations, next);
      } else {
        write_map(plan_.tilemaps[frame], next);
      }
    }
  }

 private:
  const Animation &animation_;
  const Plan &plan_;
  const Options &options_;
  int reserved_tiles_;
  std::string prefix_;
  std::vector<int> offsets_;
  std::ofstream output_;

  void section(const std::string &suffix) {
    output_ << "SECTION \"" << prefix_ << ' ' << suffix << "\", ROMX\n";
  }

  WriteGroups collect_writes(int frame, const std::vector<int> &before,
                             const std::vector<int> &after,
                             bool initialize) const {
    WriteGroups writes;
    std::vector<bool> top_slot(256, false);
    for (int location = 0; location <= 150; ++location) {
      top_slot[plan_.tilemaps[frame][location]] = true;
    }
    for (int slot = 0; slot < static_cast<int>(after.size()); ++slot) {
      if (!initialize && before[slot] == after[slot]) {
        continue;
      }
      const int index = reserved_tiles_ + slot;
      auto &groups = top_slot[index] ? writes.top : writes.bottom;
      for (int row = 0; row < kTileRows; ++row) {
        const auto value = animation_.tiles[after[slot]][row];
        if (!initialize && animation_.tiles[before[slot]][row] == value) {
          continue;
        }
        groups[value].push_back(static_cast<std::uint16_t>(
            0x8800 + index * 16 + row * 2));
        ++writes.stores;
      }
    }
    for (const auto &[value, unused] : writes.top) {
      if (writes.bottom.contains(value)) {
        writes.shared_boundary = value;
        break;
      }
    }
    writes.loads = static_cast<int>(writes.top.size() + writes.bottom.size()) -
                   (writes.shared_boundary ? 1 : 0);
    writes.cycles = writes.loads * 3 + writes.stores * 5;
    return writes;
  }

  void write_gfx_init() {
    const auto writes = collect_writes(0, plan_.assignments[0],
                                       plan_.assignments[0], true);
    int ignored_cycles = 0;
    int ignored_counter = 0;
    int ignored_scanlines = 0;
    bool ignored_dma = false;
    emit_groups(writes, false, ignored_cycles, ignored_counter,
                ignored_scanlines, ignored_dma);
    output_ << "jp update_bg_done\n";
  }

  void write_gfx_frame(int frame, int next_frame) {
    const int previous =
        (frame + static_cast<int>(plan_.assignments.size()) - 1) %
        plan_.assignments.size();
    const auto writes = collect_writes(frame, plan_.assignments[previous],
                                       plan_.assignments[frame], false);
    int total_cycles = 0;
    int cycle_counter = 0;
    int scanlines = 0;
    bool done_dma = false;
    emit_groups(writes, true, total_cycles, cycle_counter, scanlines, done_dma);

    if (!done_dma) {
      output_ << ".wait_for_oam_scanline\n"
              << "ld a, [rLY]\n"
              << "cp 30\n"
              << "jr nz, .wait_for_oam_scanline\n"
              << ".wait_for_oam_scanline_hblank\n"
              << "ld a, [rSTAT]\n"
              << "and %0000011\n"
              << "jr nz, .wait_for_oam_scanline_hblank\n"
              << "ld a, [rLCDC]\n"
              << "set 2, a\n"
              << "ld [rLCDC], a\n"
              << "ld a, HIGH(wShadowOAM2)\n"
              << "ld sp, hTempStack\n"
              << "call hOAMDMA\n";
    }

    output_ << "ld a, LOW(" << prefix_ << "_gfx" << next_frame << ")\n"
            << "ld [ptr_next_update_bg], a\n"
            << "ld a, HIGH(" << prefix_ << "_gfx" << next_frame << ")\n"
            << "ld [ptr_next_update_bg+1], a\n";
    if (options_.no_colon) {
      output_ << "ld a, LOW(BANK(" << prefix_ << "_gfx" << next_frame
              << "))\n";
    } else {
      output_ << "ld a, BANK(" << prefix_ << "_gfx" << next_frame << ")\n";
    }
    output_ << "ld [next_gfx_bank], a\n"
            << "jp update_bg_done\n";
  }

  void emit_groups(const WriteGroups &writes, bool include_halts,
                   int &total_cycles, int &cycle_counter, int &scanlines,
                   bool &done_dma) {
    const auto add_cycles = [&](int amount) {
      if (!include_halts) {
        return;
      }
      total_cycles += amount;
      if (total_cycles > 1075) {
        cycle_counter += amount;
      }
      if (cycle_counter > 44) {
        output_ << "xor a\n"
                << "ldh [rIF], a\n"
                << "halt\n";
        cycle_counter = 0;
        ++scanlines;
        if (scanlines == 31 && !done_dma) {
          output_ << "ld a, [rLCDC]\n"
                  << "set 2, a\n"
                  << "ld [rLCDC], a\n"
                  << "ld a, HIGH(wShadowOAM2)\n"
                  << "ld hl, sp+0\n"
                  << "ld sp, hTempStack\n"
                  << "call hOAMDMA\n"
                  << "ld sp, hl\n"
                  << "xor a\n"
                  << "ldh [rIF], a\n"
                  << "halt\n";
          done_dma = true;
        }
      }
    };

    const auto emit_group = [&](const auto &groups,
                                std::optional<std::uint16_t> first,
                                std::optional<std::uint16_t> last,
                                bool skip_first_load) {
      std::vector<std::uint16_t> order;
      order.reserve(groups.size());
      if (first && groups.contains(*first)) {
        order.push_back(*first);
      }
      for (const auto &[value, unused] : groups) {
        if ((!first || value != *first) && (!last || value != *last)) {
          order.push_back(value);
        }
      }
      if (last && groups.contains(*last) && (!first || *last != *first)) {
        order.push_back(*last);
      }

      bool first_value = true;
      for (const auto value : order) {
        if (!(skip_first_load && first_value)) {
          output_ << "ld sp, " << hex_value(value) << "\n";
          add_cycles(3);
        }
        for (const auto address : groups.at(value)) {
          output_ << "ld [" << hex_value(address) << "], sp\n";
          add_cycles(5);
        }
        first_value = false;
      }
    };

    emit_group(writes.top, std::nullopt, writes.shared_boundary, false);
    emit_group(writes.bottom, writes.shared_boundary, std::nullopt,
               writes.shared_boundary.has_value() && !writes.top.empty());
  }

  void write_map(const std::vector<int> &tilemap, int next_frame) {
    output_ << "ld de, 3\n"
            << "ld bc, 4\n";
    int old_offset = -1;
    for (int location = 0; location < kMapWidth * kMapHeight; ++location) {
      const int offset = offsets_[location];
      if (old_offset >= 0 && offset - old_offset == 3) {
        output_ << "add hl, de\n";
      } else if (old_offset >= 0 && offset - old_offset == 4) {
        output_ << "add hl, bc\n";
      } else {
        output_ << "ld hl, playfield_buffer + " << hex_value(offset) << "\n";
      }
      output_ << "ld [hl], " << (tilemap[location] ^ 0x80) << "\n";
      old_offset = offset;
    }
    write_map_tail(next_frame);
  }

  void write_menu_map_diff(int frame, const std::vector<int> &locations,
                           int next_frame) {
    std::map<int, std::vector<int>> by_tile;
    for (const int location : locations) {
      by_tile[plan_.tilemaps[frame][location]].push_back(location);
    }
    for (const auto &[tile, tile_locations] : by_tile) {
      output_ << "ld a, " << (tile ^ 0x80) << "\n";
      for (const int location : tile_locations) {
        const int x = location % kMapWidth;
        const int y = location / kMapWidth;
        const int address = 0x9800 + y * 32 + x;
        output_ << "ld [" << hex_value(address) << "], a\n";
      }
    }
    write_map_tail(next_frame);
  }

  void write_map_tail(int next_frame) {
    output_ << "ld a, LOW(" << prefix_ << "_map" << next_frame << ")\n"
            << "ld [update_playfield_buffer+1], a\n"
            << "ld a, HIGH(" << prefix_ << "_map" << next_frame << ")\n"
            << "ld [update_playfield_buffer+2], a\n"
            << "ld a, BANK(" << prefix_ << "_map" << next_frame
            << ") & $FF\n"
            << "ld [next_map_bank], a\n"
            << "ret\n";
  }
};

void print_metrics(const Animation &animation, const Plan &plan,
                   bool menu_mode) {
  long long replacements = 0;
  long long stores = 0;
  long long loads = 0;
  long long cycles = 0;
  int max_replacements = 0;
  int max_stores = 0;
  int max_cycles = 0;
  int max_cycle_frame = 0;
  for (int frame = 0; frame < static_cast<int>(plan.metrics.size()); ++frame) {
    const auto &metric = plan.metrics[frame];
    replacements += metric.replacements;
    stores += metric.row_stores;
    loads += metric.row_loads;
    cycles += metric.gfx_cycles;
    max_replacements = std::max(max_replacements, metric.replacements);
    max_stores = std::max(max_stores, metric.row_stores);
    if (metric.gfx_cycles > max_cycles) {
      max_cycles = metric.gfx_cycles;
      max_cycle_frame = frame;
    }
  }

  std::cout << animation.frames.size() << " generated frames, "
            << animation.tiles.size() << " unique tiles\n";
  if (!menu_mode) {
    const double count = static_cast<double>(animation.frames.size());
    std::cout << "tile replacements: total=" << replacements
              << " mean=" << std::fixed << std::setprecision(2)
              << replacements / count << " max=" << max_replacements << '\n'
              << "16-bit row stores: total=" << stores << " mean="
              << stores / count << " max=" << max_stores << '\n'
              << "graphics M-cycles: total=" << cycles << " mean="
              << cycles / count << " max=" << max_cycles << " (frame "
              << max_cycle_frame << ")\n";
  }
}

}  // namespace

int main(int argc, char **argv) {
  try {
    const Options options = parse_options(argc, argv);
    const auto pixels = load_gif_frames(
        options.input, !options.menu_mode && !options.no_colon);
    const Animation animation = make_animation(pixels);
    ResidencyPlanner planner(animation,
                             options.menu_mode ? 0 : kNormalReservedTiles);
    const Plan plan = planner.make_plan(options.menu_mode);
    AssemblyWriter writer(animation, plan, options);
    writer.write();
    print_metrics(animation, plan, options.menu_mode);
    return 0;
  } catch (const std::exception &error) {
    std::cerr << "gif2tiles: " << error.what() << '\n';
    return 1;
  }
}
