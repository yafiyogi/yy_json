#include <string>
#include <optional>
#include <vector>

#include "boost/json/basic_parser_impl.hpp"
#include "fmt/format.h"
#include "gtest/gtest.h"

#include "yy_json_pointer_util.h"

namespace yafiyogi::yy_cpp::tests {

using namespace std::string_view_literals;

class TestJsonPointerUtil:
      public testing::Test
{
  public:
    void SetUp() override
    {
    }

    void TearDown() override
    {
    }
};

TEST_F(TestJsonPointerUtil, trim)
{
  {
    std::string_view json_pointer{"/abc/def"};

    EXPECT_EQ(json_pointer, yy_json::json_pointer_trim(json_pointer));
  }

  {
    std::string_view json_pointer{"/abc/def/"};

    EXPECT_EQ("/abc/def"sv, yy_json::json_pointer_trim(json_pointer));
  }

  {
    std::string_view json_pointer{"  /abc/def/  "};

    EXPECT_EQ("/abc/def"sv, yy_json::json_pointer_trim(json_pointer));
  }
}

TEST_F(TestJsonPointerUtil, tokenize)
{
  {
    auto path = yy_json::json_pointer_tokenize("/abc/def");
    ASSERT_EQ(size_t{2}, path.size());
    EXPECT_EQ("abc"sv, path[0]);
    EXPECT_EQ("def"sv, path[1]);
  }

  {
    auto path = yy_json::json_pointer_tokenize("/abc/def/");
    ASSERT_EQ(size_t{2}, path.size());
    EXPECT_EQ("abc"sv, path[0]);
    EXPECT_EQ("def"sv, path[1]);
  }

  {
    auto path = yy_json::json_pointer_tokenize("abc/def/");
    ASSERT_EQ(size_t{2}, path.size());
    EXPECT_EQ("abc"sv, path[0]);
    EXPECT_EQ("def"sv, path[1]);
  }
}


} // namespace yafiyogi::yy_cpp::tests
