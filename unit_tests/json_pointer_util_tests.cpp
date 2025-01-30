/*

  MIT License

  Copyright (c) 2024-2025 Yafiyogi

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.

*/

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
