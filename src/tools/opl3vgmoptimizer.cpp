#include <algorithm>
#include <cstdint>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <optional>
#include <stdexcept>
#include <string>
#include <vector>

namespace {

using Bytes = std::vector<std::uint8_t>;

std::uint32_t read_u32(const Bytes &data, std::size_t offset) {
  if (offset + 4 > data.size()) {
    throw std::runtime_error("truncated VGM header");
  }
  return static_cast<std::uint32_t>(data[offset]) |
         (static_cast<std::uint32_t>(data[offset + 1]) << 8) |
         (static_cast<std::uint32_t>(data[offset + 2]) << 16) |
         (static_cast<std::uint32_t>(data[offset + 3]) << 24);
}

void write_u32(Bytes &data, std::size_t offset, std::uint32_t value) {
  if (offset + 4 > data.size()) {
    throw std::runtime_error("internal VGM header write overflow");
  }
  for (int byte = 0; byte < 4; ++byte) {
    data[offset + byte] = static_cast<std::uint8_t>(value >> (byte * 8));
  }
}

Bytes read_file(const std::filesystem::path &path) {
  std::ifstream input(path, std::ios::binary | std::ios::ate);
  if (!input) {
    throw std::runtime_error("could not open " + path.string());
  }
  const auto end = input.tellg();
  if (end < 0) {
    throw std::runtime_error("could not determine the size of " +
                             path.string());
  }
  Bytes data(static_cast<std::size_t>(end));
  input.seekg(0);
  input.read(reinterpret_cast<char *>(data.data()),
             static_cast<std::streamsize>(data.size()));
  if (!input) {
    throw std::runtime_error("could not read " + path.string());
  }
  return data;
}

void write_file(const std::filesystem::path &path, const Bytes &data) {
  std::ofstream output(path, std::ios::binary);
  if (!output) {
    throw std::runtime_error("could not open " + path.string());
  }
  output.write(reinterpret_cast<const char *>(data.data()),
               static_cast<std::streamsize>(data.size()));
  if (!output) {
    throw std::runtime_error("could not write " + path.string());
  }
}

std::size_t vgm_data_offset(const Bytes &data) {
  if (data.size() < 0x40 ||
      !std::equal(data.begin(), data.begin() + 4, "Vgm ")) {
    throw std::runtime_error("input is not an uncompressed VGM file");
  }
  if (read_u32(data, 0x08) < 0x150) {
    return 0x40;
  }
  const auto relative = read_u32(data, 0x34);
  return relative == 0 ? 0x40 : 0x34 + relative;
}

std::size_t command_size(const Bytes &data, std::size_t offset) {
  if (offset >= data.size()) {
    throw std::runtime_error("VGM command stream has no end command");
  }

  const auto command = data[offset];
  if ((command >= 0x30 && command <= 0x3f) || command == 0x4f ||
      command == 0x50 || command == 0x94) {
    return 2;
  }
  if ((command >= 0x51 && command <= 0x5f) || command == 0x61 ||
      (command >= 0xa0 && command <= 0xbf)) {
    return 3;
  }
  if (command == 0x62 || command == 0x63 || command == 0x66 ||
      (command >= 0x70 && command <= 0x8f)) {
    return 1;
  }
  if (command == 0x64 || (command >= 0xc0 && command <= 0xd6)) {
    return 4;
  }
  if (command == 0x90 || command == 0x91 || command == 0x95 ||
      command == 0xe0 || command == 0xe1) {
    return 5;
  }
  if (command == 0x92) {
    return 6;
  }
  if (command == 0x93) {
    return 11;
  }
  if (command == 0x68) {
    return 12;
  }
  if (command == 0x67) {
    if (offset + 7 > data.size() || data[offset + 1] != 0x66) {
      throw std::runtime_error("malformed VGM data block");
    }
    return 7 + read_u32(data, offset + 3);
  }

  constexpr char digits[] = "0123456789ABCDEF";
  std::string hex(2, '0');
  hex[0] = digits[command >> 4];
  hex[1] = digits[command & 15];
  throw std::runtime_error("unsupported VGM command 0x" + hex);
}

struct Result {
  Bytes data;
  std::size_t opl_writes = 0;
  std::size_t dummy_writes_removed = 0;
  std::size_t port_zero_removed = 0;
  std::size_t port_one_removed = 0;
};

Result optimize(const Bytes &input) {
  const auto data_offset = vgm_data_offset(input);
  if (data_offset > input.size()) {
    throw std::runtime_error("VGM data offset is outside the file");
  }

  const auto loop_relative = read_u32(input, 0x1c);
  const std::optional<std::size_t> old_loop =
      loop_relative == 0
          ? std::nullopt
          : std::optional<std::size_t>(0x1c + loop_relative);
  std::optional<std::size_t> new_loop;

  Result result;
  result.data.assign(input.begin(), input.begin() + data_offset);

  std::size_t cursor = data_offset;
  std::size_t old_stream_end = 0;
  while (cursor < input.size()) {
    if (old_loop && cursor == *old_loop) {
      new_loop = result.data.size();
    }

    const auto size = command_size(input, cursor);
    if (cursor + size > input.size()) {
      throw std::runtime_error("truncated VGM command");
    }
    const auto command = input[cursor];
    const bool opl_write = command == 0x5e || command == 0x5f;
    // $36-$3F are unimplemented in each OPL register bank. Old Furnace
    // emitted writes to $3F solely to consume time during hard resets.
    const bool dummy_write = opl_write && input[cursor + 1] == 0x3f;
    if (opl_write) {
      ++result.opl_writes;
    }
    if (dummy_write) {
      ++result.dummy_writes_removed;
      if (command == 0x5e) {
        ++result.port_zero_removed;
      } else {
        ++result.port_one_removed;
      }
    } else {
      result.data.insert(result.data.end(), input.begin() + cursor,
                         input.begin() + cursor + size);
    }

    cursor += size;
    if (command == 0x66) {
      old_stream_end = cursor;
      break;
    }
  }
  if (old_stream_end == 0) {
    throw std::runtime_error("VGM command stream has no end command");
  }

  const auto new_stream_end = result.data.size();
  result.data.insert(result.data.end(), input.begin() + old_stream_end,
                     input.end());

  const auto gd3_relative = read_u32(input, 0x14);
  if (gd3_relative != 0) {
    const auto old_gd3 = static_cast<std::size_t>(0x14 + gd3_relative);
    if (old_gd3 < old_stream_end || old_gd3 > input.size()) {
      throw std::runtime_error("VGM GD3 offset is outside the file suffix");
    }
    const auto new_gd3 = new_stream_end + (old_gd3 - old_stream_end);
    write_u32(result.data, 0x14,
              static_cast<std::uint32_t>(new_gd3 - 0x14));
  }

  if (old_loop) {
    if (!new_loop) {
      throw std::runtime_error("VGM loop offset is not on a command boundary");
    }
    write_u32(result.data, 0x1c,
              static_cast<std::uint32_t>(*new_loop - 0x1c));
  }
  write_u32(result.data, 0x04,
            static_cast<std::uint32_t>(result.data.size() - 4));
  return result;
}

}  // namespace

int main(int argc, char **argv) {
  if (argc != 3) {
    std::cerr << "usage: " << argv[0] << " INPUT.vgm OUTPUT.vgm\n";
    return 2;
  }

  try {
    const auto input = read_file(argv[1]);
    const auto result = optimize(input);
    write_file(argv[2], result.data);

    std::cout << "OPL writes: " << result.opl_writes << '\n'
              << "unused $3F writes removed: "
              << result.dummy_writes_removed << " (port 0: "
              << result.port_zero_removed << ", port 1: "
              << result.port_one_removed << ")\n"
              << "bytes: " << input.size() << " -> " << result.data.size()
              << '\n';
  } catch (const std::exception &error) {
    std::cerr << "opl3vgmoptimizer: " << error.what() << '\n';
    return 1;
  }
}
