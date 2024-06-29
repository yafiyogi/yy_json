#include <string>
#include <optional>
#include <vector>

#include "boost/json/basic_parser_impl.hpp"
#include "fmt/format.h"
#include "gtest/gtest.h"

#include "yy_json_pointer.h"

namespace yafiyogi::yy_cpp::tests {

class TestJsonPointer:
      public testing::Test
{
  public:
    using jp_builder = yy_json::json_pointer_builder<int>;
    using value_type = jp_builder::value_type;
    using json_handler = jp_builder::handler_type;
    using json_scope = json_handler::scope_type;
    using json_parser = boost::json::basic_parser<json_handler>;

    class visitor:
      public json_handler::visitor_type
    {
      public:
        virtual void apply(const json_scope & /* scope */,
                           value_type & /* payload */,
                           std::string_view str )
        {
          ASSERT_TRUE(m_str.has_value());
          EXPECT_EQ(str, m_str.value());
        }

        virtual void apply(const json_scope & /* scope */,
                           value_type & /* payload */,
                           std::string_view /* raw */,
                           std::int64_t num)
        {
          ASSERT_TRUE(m_int64.has_value());
          EXPECT_EQ(num, m_int64.value());
        }

        virtual void apply(const json_scope & /* scope */,
                           value_type & /* payload */,
                           std::string_view /* raw */,
                           std::uint64_t num )
        {
          ASSERT_TRUE(m_uint64.has_value());
          EXPECT_EQ(num, m_uint64.value());
        }

        virtual void apply(const json_scope & /* scope */,
                           value_type & /* payload */,
                           std::string_view /* raw */,
                           double num)
        {
          ASSERT_TRUE(m_double.has_value());
          EXPECT_EQ(num, m_double.value());
        }

        virtual void apply(const json_scope & /* scope */,
                           value_type & /* payload */,
                           bool flag)
        {
          ASSERT_TRUE(m_bool.has_value());
          EXPECT_EQ(flag, m_bool.value());
        }

        std::optional<std::string_view> m_str;
        std::optional<int64_t> m_int64;
        std::optional<uint64_t> m_uint64;
        std::optional<double> m_double;
        std::optional<bool> m_bool;
    };

    void SetUp() override
    {
        m_visitor = std::make_unique<visitor>();
    }

    void TearDown() override
    {
    }

    static std::string & prepare_json(std::string & js)
    {
      std::replace(js.begin(), js.end(), '\'', '"');

      return js;
    }

    std::unique_ptr<visitor> m_visitor;
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

  auto config = builder.create();

  auto root = config.pointers.root();

  size_t idx = 0;
  root->visit([&res, &idx](auto & label, auto) {
    EXPECT_EQ(label, res[idx]);
    ++idx;
  });

  EXPECT_TRUE(nullptr != config.pointers.find_pointer("/abc"));
  EXPECT_TRUE(nullptr != config.pointers.find_pointer("/def"));
  EXPECT_TRUE(nullptr != config.pointers.find_pointer("/ghi"));
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

  auto config = builder.create();

  auto root = config.pointers.root();

  size_t idx = 0;
  root->visit([&res, &idx](auto & label, auto) {
    EXPECT_EQ(label, res[idx]);
    ++idx;
  });

  EXPECT_TRUE(nullptr != config.pointers.find_pointer("/abc"));
  EXPECT_TRUE(nullptr != config.pointers.find_pointer("/def"));
  EXPECT_TRUE(nullptr != config.pointers.find_pointer("/ghi"));
}

TEST_F(TestJsonPointer, SimpleString)
{
  boost::json::error_code ec{};
  auto options = boost::json::parse_options{};
  jp_builder builder{};

  builder.add_pointer("/abc", 668);

  m_visitor->m_str = "eight eight eight";

  std::string js = fmt::format("{{ 'abc' : '{}'}}", m_visitor->m_str.value());
  prepare_json(js);

  json_parser parser{options, builder.create(options.max_depth)};
  parser.handler().set_visitor(std::move(m_visitor));

  parser.write_some(false, js.data(), js.size(), ec);
}

TEST_F(TestJsonPointer, SimpleInt64)
{
  boost::json::error_code ec{};
  auto options = boost::json::parse_options{};
  jp_builder builder{};

  builder.add_pointer("/abc", 668);

  m_visitor->m_int64 = 888;

  std::string js = fmt::format("{{ 'abc' : {} }}", m_visitor->m_int64.value());
  prepare_json(js);

  json_parser parser{options, builder.create(options.max_depth)};
  parser.handler().set_visitor(std::move(m_visitor));

  parser.write_some(false, js.data(), js.size(), ec);
}

TEST_F(TestJsonPointer, SimpleDouble)
{
  boost::json::error_code ec{};
  auto options = boost::json::parse_options{};
  jp_builder builder{};

  builder.add_pointer("/abc", 668);

  m_visitor->m_double = 888.1;

  std::string js = fmt::format("{{ 'abc' : {} }}", m_visitor->m_double.value());
  prepare_json(js);

  json_parser parser{options, builder.create(options.max_depth)};
  parser.handler().set_visitor(std::move(m_visitor));

  parser.write_some(false, js.data(), js.size(), ec);
}

TEST_F(TestJsonPointer, SimpleBool)
{
  boost::json::error_code ec{};
  auto options = boost::json::parse_options{};
  jp_builder builder{};

  builder.add_pointer("/abc", 668);

  m_visitor->m_bool = true;

  std::string js = fmt::format("{{ 'abc' : {} }}", m_visitor->m_bool.value() ? "true" : "false");
  prepare_json(js);

  json_parser parser{options, builder.create(options.max_depth)};
  parser.handler().set_visitor(std::move(m_visitor));

  parser.write_some(false, js.data(), js.size(), ec);
}


TEST_F(TestJsonPointer, DocString)
{
  boost::json::error_code ec{};
  auto options = boost::json::parse_options{};
  jp_builder builder{};

  builder.add_pointer("", 668);

  m_visitor->m_str = "eight eight eight";

  std::string js = fmt::format("'{}'", m_visitor->m_str.value());
  prepare_json(js);

  json_parser parser{options, builder.create(options.max_depth)};
  parser.handler().set_visitor(std::move(m_visitor));

  parser.write_some(false, js.data(), js.size(), ec);
}

TEST_F(TestJsonPointer, DocInt64)
{
  boost::json::error_code ec{};
  auto options = boost::json::parse_options{};
  jp_builder builder{};

  builder.add_pointer("", 668);

  m_visitor->m_int64 = 888;

  std::string js = fmt::format("{}", m_visitor->m_int64.value());
  prepare_json(js);

  json_parser parser{options, builder.create(options.max_depth)};
  parser.handler().set_visitor(std::move(m_visitor));

  parser.write_some(false, js.data(), js.size(), ec);
}

TEST_F(TestJsonPointer, DocDouble)
{
  boost::json::error_code ec{};
  auto options = boost::json::parse_options{};
  jp_builder builder{};

  builder.add_pointer("", 668);

  m_visitor->m_double = 888.1;

  std::string js = fmt::format("{}", m_visitor->m_double.value());
  prepare_json(js);

  json_parser parser{options, builder.create(options.max_depth)};
  parser.handler().set_visitor(std::move(m_visitor));

  parser.write_some(false, js.data(), js.size(), ec);
}

TEST_F(TestJsonPointer, DocBool)
{
  boost::json::error_code ec{};
  auto options = boost::json::parse_options{};
  jp_builder builder{};

  builder.add_pointer("", 668);

  m_visitor->m_bool = true;

  std::string js = fmt::format("{}", m_visitor->m_bool.value() ? "true" : "false");
  prepare_json(js);

  json_parser parser{options, builder.create(options.max_depth)};
  parser.handler().set_visitor(std::move(m_visitor));

  parser.write_some(false, js.data(), js.size(), ec);
}

} // namespace yafiyogi::yy_cpp::tests
