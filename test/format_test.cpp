// Copyright 2018 William Jagels
#include "format.hpp"
#include <gtest/gtest.h>
#include <type_traits>

using namespace wijagels;

template <typename T, typename... Rs>
constexpr auto valid(bool)
    -> decltype(std::declval<T>().format(std::declval<Rs>()...), true) {
  return true;
}

template <typename T, typename... Rs>
constexpr bool valid(...) {
  return false;
}

template <typename T, typename... Rs>
constexpr bool is_valid_format() {
  return valid<T, Rs...>(bool{});
}

TEST(Formatter, Simple) {
  constexpr auto f = "hello, {1}! I am {0}."_f;
  EXPECT_EQ("hello, world! I am gentoo.", f.format("gentoo", "world"));
  EXPECT_EQ("hello hello", "{0} {0}"_f.format("hello"));
}

TEST(Formatter, Sfinae) {
  EXPECT_TRUE(
      (is_valid_format<decltype("{} {}"_f), const char *, const char *>()));
  EXPECT_TRUE((is_valid_format<decltype("{0} {0}"_f), const char *>()));
  EXPECT_FALSE((is_valid_format<decltype("{} {}"_f), const char *>()));
}
