// Copyright 2018 William Jagels
#pragma once
#include <array>
#include <sstream>
#include <string_view>
#include <tuple>
#include <utility>

namespace wijagels {
template <std::size_t I, std::size_t B, std::size_t E>
struct idx_set {
  static constexpr std::size_t arg = I;
  static constexpr std::size_t begin = B;
  static constexpr std::size_t end = E;
};

template <std::size_t Size, typename Indexes>
class format_string {
  template <std::size_t... Is>
  static constexpr bool is_valid(std::size_t args, std::index_sequence<Is...>) {
    return std::max(std::tuple_element<Is, Indexes>::type::arg...) + 1 == args;
  }

  template <std::size_t Idx>
  static constexpr std::size_t lastelem() {
    if constexpr (Idx > 0)
      return std::tuple_element<(Idx > 0 ? Idx - 1 : 0), Indexes>::type::end;
    else
      return 0;
  }

 public:
  constexpr format_string(std::array<char, Size> arr) : m_arr{std::move(arr)} {}

  template <typename... Ts,
            typename = std::enable_if_t<is_valid(
                sizeof...(Ts),
                std::make_index_sequence<std::tuple_size_v<Indexes>>{})>>
  auto format(Ts &&... ts) const {
    std::ostringstream os{};
    auto ptr = this->m_arr.data();
    constexpr auto TupSz = std::tuple_size_v<Indexes>;
    format_impl(os, ptr, std::forward_as_tuple(ts...),
                std::make_index_sequence<TupSz>{});
    return os.str();
  }

 private:
  template <typename Tuple, std::size_t... Is>
  constexpr void format_impl(std::ostringstream &os, const char *pos,
                             const Tuple &tup,
                             std::index_sequence<Is...>) const {
    ((os << std::string_view{pos + lastelem<Is>(),
                             std::tuple_element<Is, Indexes>::type::begin -
                                 lastelem<Is>()}
         << std::get<std::tuple_element<Is, Indexes>::type::arg>(tup)),
     ...);
    os << std::string_view{pos + lastelem<sizeof...(Is)>(),
                           Size - lastelem<sizeof...(Is)>()};
  }

  const std::array<char, Size> m_arr;
};

template <std::size_t Idx = 0, std::size_t Number = 0, std::size_t BeginIdx = 0,
          bool Braces = false, std::size_t OrigNum = 0, bool SeenNum = false>
struct parser_state {
  template <char C>
  static constexpr auto process_token() {
    if constexpr (C == '{') {
      return std::pair<parser_state<Idx + 1, Number, Idx, true, OrigNum>,
                       std::tuple<>>{};
    } else if constexpr (Braces) {
      if constexpr (C <= '9' && C >= '0') {
        if constexpr (SeenNum)
          return std::pair<parser_state<Idx + 1, Number * 10 + (C - '0'),
                                        BeginIdx, Braces, OrigNum, true>,
                           std::tuple<>>{};
        else
          return std::pair<
              parser_state<Idx + 1, C - '0', BeginIdx, Braces, OrigNum, true>,
              std::tuple<>>{};
      } else if constexpr (C == '}') {
        return std::pair<
            parser_state<Idx + 1, OrigNum + 1, Idx, false, OrigNum + 1>,
            std::tuple<idx_set<Number, BeginIdx, Idx + 1>>>{};
      } else {
        throw "Invalid characters within {}";
      }
    } else {
      return std::pair<parser_state<Idx + 1, Number, Idx, false, OrigNum>,
                       std::tuple<>>{};
    }
  }
};

template <typename State, char C, char... S>
constexpr auto parse_format_impl() {
  auto[new_state, tup] = State::template process_token<C>();
  if constexpr (sizeof...(S) > 0)
    return std::tuple_cat(tup, parse_format_impl<decltype(new_state), S...>());
  else
    return tup;
}

template <char... S>
constexpr auto parse_format() {
  return parse_format_impl<parser_state<>, S...>();
}

template <typename Char, Char... S>
constexpr auto operator""_f() {
  auto idxs = parse_format<S...>();
  return format_string<sizeof...(S), decltype(idxs)>{{{S...}}};
}

}  // namespace wijagels
