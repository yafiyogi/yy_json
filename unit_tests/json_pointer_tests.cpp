#include <string>
#include <optional>
#include <vector>

#include "boost/json/basic_parser_impl.hpp"
#include "fmt/format.h"
#include "gtest/gtest.h"

#include "yy_json_pointer.h"

namespace yafiyogi::yy_cpp::tests {

using namespace std::string_view_literals;

class TestJsonPointer:
      public testing::Test
{
  public:
    using value_type = int;

    class test_visitor
    {
      public:
        test_visitor() noexcept = default;
        test_visitor(const test_visitor &) noexcept = default;
        test_visitor(test_visitor &&) noexcept = default;
        virtual ~test_visitor() noexcept = default;

        test_visitor & operator=(const test_visitor &) noexcept = default;
        test_visitor & operator=(test_visitor &&) noexcept = default;

        virtual void apply_str(value_type & /* payload */,
                               std::string_view str)
        {
          ASSERT_TRUE(m_str.has_value());
          EXPECT_EQ(str, m_str.value());
        }

        virtual void apply_int64(value_type & /* payload */,
                                 std::string_view /* raw */,
                                 std::int64_t num)
        {
          ASSERT_TRUE(m_int64.has_value());
          EXPECT_EQ(num, m_int64.value());
        }

        virtual void apply_uint64(value_type & /* payload */,
                                  std::string_view /* raw */,
                                  std::uint64_t num)
        {
          ASSERT_TRUE(m_uint64.has_value());
          EXPECT_EQ(num, m_uint64.value());
        }

        virtual void apply_double(value_type & /* payload */,
                                  std::string_view /* raw */,
                                  double num)
        {
          ASSERT_TRUE(m_double.has_value());
          EXPECT_EQ(num, m_double.value());
        }

        virtual void apply_bool(value_type & /* payload */,
                           bool flag)
        {
          ASSERT_TRUE(m_bool.has_value());
          EXPECT_EQ(flag, m_bool.value());
        }

        std::optional<std::string_view> m_str{};
        std::optional<int64_t> m_int64{};
        std::optional<uint64_t> m_uint64{};
        std::optional<double> m_double{};
        std::optional<bool> m_bool{};
        int count = 0;
    };

    using jp_builder = yy_json::json_pointer_builder<int, test_visitor>;
    using json_handler = jp_builder::handler_type;
    using json_parser = boost::json::basic_parser<json_handler>;

    void SetUp() override
    {
    }

    void TearDown() override
    {
    }

    static std::string & prepare_json(std::string & js)
    {
      std::replace(js.begin(), js.end(), '\'', '"');

      return js;
    }
};

TEST_F(TestJsonPointer, Add)
{
  jp_builder builder{};

  std::vector<std::string> vec = {"/abc", "/def", "/ghi"};
  std::vector<std::string> res = {"abc", "def", "ghi"};
  int val = 668;

  for(auto & v : vec)
  {
    builder.add_pointer(v, val++);
  }

  auto options = boost::json::parse_options{};
  auto config = builder.create(options.max_depth);

  auto root = config.pointers.root();

  size_t idx = 0;
  root->visit([&res, &idx](auto & label, auto) {
    EXPECT_EQ(label, res[idx]);
    ++idx;
  });

  EXPECT_TRUE(nullptr != config.pointers.find_pointer("/abc"sv));
  EXPECT_TRUE(nullptr != config.pointers.find_pointer("/def"sv));
  EXPECT_TRUE(nullptr != config.pointers.find_pointer("/ghi"sv));
}

TEST_F(TestJsonPointer, AddOutOfOrder)
{
  jp_builder builder{};

  std::vector<std::string> vec = {"/ghi", "/abc", "/def"};
  std::vector<std::string> res = {"abc", "def", "ghi"};
  int val = 668;

  for(auto & v : vec)
  {
    builder.add_pointer(v, val++);
  }

  auto options = boost::json::parse_options{};
  auto config = builder.create(options.max_depth);

  auto root = config.pointers.root();

  size_t idx = 0;
  root->visit([&res, &idx](auto & label, auto) {
    EXPECT_EQ(label, res[idx]);
    ++idx;
  });

  EXPECT_TRUE(nullptr != config.pointers.find_pointer("/abc"sv));
  EXPECT_TRUE(nullptr != config.pointers.find_pointer("/def"sv));
  EXPECT_TRUE(nullptr != config.pointers.find_pointer("/ghi"sv));
}

TEST_F(TestJsonPointer, SimpleString)
{
  boost::json::error_code ec{};
  auto options = boost::json::parse_options{};
  jp_builder builder{};

  builder.add_pointer("/abc"sv, 668);

  auto v_str = "eight eight eight"sv;

  std::string js = fmt::format("{{ 'abc' : '{}'}}"sv, v_str);
  prepare_json(js);

  json_parser parser{options, builder.create(options.max_depth)};

  parser.handler().visitor().m_str = v_str;

  parser.write_some(false, js.data(), js.size(), ec);
}

TEST_F(TestJsonPointer, SimpleInt64)
{
  boost::json::error_code ec{};
  auto options = boost::json::parse_options{};
  jp_builder builder{};

  builder.add_pointer("/abc"sv, 668);

  int64_t v_int64 = 888;

  std::string js = fmt::format("{{ 'abc' : {} }}"sv, v_int64);
  prepare_json(js);

  json_parser parser{options, builder.create(options.max_depth)};

  parser.handler().visitor().m_int64 = v_int64;

  parser.write_some(false, js.data(), js.size(), ec);
}

TEST_F(TestJsonPointer, SimpleDouble)
{
  boost::json::error_code ec{};
  auto options = boost::json::parse_options{};
  jp_builder builder{};

  builder.add_pointer("/abc"sv, 668);

  double v_double = 888.1;

  std::string js = fmt::format("{{ 'abc' : {} }}"sv, v_double);
  prepare_json(js);

  json_parser parser{options, builder.create(options.max_depth)};

  parser.handler().visitor().m_double = v_double;

  parser.write_some(false, js.data(), js.size(), ec);
}

TEST_F(TestJsonPointer, SimpleBool)
{
  boost::json::error_code ec{};
  auto options = boost::json::parse_options{};
  jp_builder builder{};

  builder.add_pointer("/abc"sv, 668);

  bool v_bool = true;

  std::string js = fmt::format("{{ 'abc' : {} }}"sv, v_bool ? "true" : "false");
  prepare_json(js);

  json_parser parser{options, builder.create(options.max_depth)};

  parser.handler().visitor().m_bool = v_bool;

  parser.write_some(false, js.data(), js.size(), ec);
}


TEST_F(TestJsonPointer, DocString)
{
  boost::json::error_code ec{};
  auto options = boost::json::parse_options{};
  jp_builder builder{};

  builder.add_pointer("", 668);

  auto v_str = "eight eight eight"sv;

  std::string js = fmt::format("'{}'"sv, v_str);
  prepare_json(js);

  json_parser parser{options, builder.create(options.max_depth)};

  parser.handler().visitor().m_str = v_str;

  parser.write_some(false, js.data(), js.size(), ec);
}

TEST_F(TestJsonPointer, DocInt64)
{
  boost::json::error_code ec{};
  auto options = boost::json::parse_options{};
  jp_builder builder{};

  builder.add_pointer("", 668);

  int64_t v_int64 = 888;

  std::string js = fmt::format("{}"sv, v_int64);
  prepare_json(js);

  json_parser parser{options, builder.create(options.max_depth)};

  parser.handler().visitor().m_int64 = v_int64;

  parser.write_some(false, js.data(), js.size(), ec);
}

TEST_F(TestJsonPointer, DocDouble)
{
  boost::json::error_code ec{};
  auto options = boost::json::parse_options{};
  jp_builder builder{};

  builder.add_pointer("", 668);

  double v_double = 888.1;

  std::string js = fmt::format("{}", v_double);
  prepare_json(js);

  json_parser parser{options, builder.create(options.max_depth)};

  parser.handler().visitor().m_double = v_double;

  parser.write_some(false, js.data(), js.size(), ec);
}

TEST_F(TestJsonPointer, DocBool)
{
  boost::json::error_code ec{};
  auto options = boost::json::parse_options{};
  jp_builder builder{};

  builder.add_pointer("", 668);

  bool v_bool = true;

  std::string js = fmt::format("{}", v_bool ? "true" : "false");
  prepare_json(js);

  json_parser parser{options, builder.create(options.max_depth)};

  parser.handler().visitor().m_bool = v_bool;

  parser.write_some(false, js.data(), js.size(), ec);
}

TEST_F(TestJsonPointer, IgnoreNestedString)
{
  class visitor_ns final:
            public test_visitor
  {
    public:
      void apply_str(value_type & /* payload */,
                     std::string_view /* str */) override
      {
        ++count;
      }
  };

  using builder_type = yy_json::json_pointer_builder<int, visitor_ns>;
  using handler_type = builder_type::handler_type;
  using parser_type = boost::json::basic_parser<handler_type>;

  boost::json::error_code ec{};
  auto options = boost::json::parse_options{};
  builder_type builder{};
  builder.add_pointer("/abc", 668);

  std::string js = fmt::format("{{ 'abc' : '777', 'nested' : {{ 'abc' : '888' }} }}");
  prepare_json(js);

  parser_type parser{options, builder.create(options.max_depth)};

  parser.write_some(false, js.data(), js.size(), ec);

  EXPECT_EQ(1, parser.handler().visitor().count);
}

TEST_F(TestJsonPointer, MatchNestedString)
{
  class visitor_ns final:
            public test_visitor
  {
    public:
      void apply_str(value_type & /* payload */,
                     std::string_view /* str */) override
      {
        ++count;
      }
  };

  using builder_type = yy_json::json_pointer_builder<int, visitor_ns>;
  using handler_type = builder_type::handler_type;
  using parser_type = boost::json::basic_parser<handler_type>;

  boost::json::error_code ec{};
  auto options = boost::json::parse_options{};
  builder_type builder{};
  builder.add_pointer("/nested/abc", 668);

  std::string js = fmt::format("{{ 'abc' : '777', 'nested' : {{ 'abc' : '888' }} }}");
  prepare_json(js);

  parser_type parser{options, builder.create(options.max_depth)};

  parser.write_some(false, js.data(), js.size(), ec);

  EXPECT_EQ(1, parser.handler().visitor().count);
}



} // namespace yafiyogi::yy_cpp::tests
