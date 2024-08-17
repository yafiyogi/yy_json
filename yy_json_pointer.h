/*

  MIT License

  Copyright (c) 2024 Yafiyogi

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

#pragma once

#include <cstddef>
#include <cstdint>

#include <limits>
#include <memory>
#include <string>
#include <string_view>
#include <type_traits>

#include "boost/json/basic_parser_impl.hpp"
#include "fmt/core.h"
#include "fmt/compile.h"

#include "yy_cpp/yy_int_util.h"
#include "yy_cpp/yy_fm_flat_trie_ptr.h"
#include "yy_cpp/yy_span.h"
#include "yy_cpp/yy_tokenizer.h"
#include "yy_cpp/yy_type_traits.h"
#include "yy_cpp/yy_vector.h"

#include "yy_json_constants.h"
#include "yy_json_pointer_util.h"

namespace yafiyogi::yy_json {

using namespace std::string_view_literals;

namespace json_pointer_detail {

template<typename LabelType,
         typename ValueType>
class Query final
{
  public:
    using traits = yy_data::fm_flat_trie_ptr_detail::trie_ptr_traits<LabelType, ValueType>;
    using label_type = typename traits::label_type;
    using node_type = typename traits::node_type;
    using value_type = typename traits::value_type;
    using size_type = typename traits::size_type;
    using trie_vector = typename traits::trie_vector;
    using data_vector = typename traits::data_vector;

    struct state_type final
    {
        size_type level{};
        node_type * state{};
    };
    using states_type = yy_quad::simple_vector<state_type>;

    constexpr explicit Query(trie_vector && p_nodes,
                             data_vector && p_data) noexcept:
      m_nodes(std::move(p_nodes)),
      m_data(std::move(p_data))
    {
    }

    Query() noexcept = default;
    Query(const Query &) = delete;
    constexpr Query(Query &&) noexcept = default;

    Query & operator=(const Query &) = delete;
    constexpr Query & operator=(Query &&) noexcept = default;

    [[nodiscard]]
    constexpr node_type * find_level(std::string_view key, node_type * scope) noexcept
    {
      node_type * state{};
      if(nullptr != scope)
      {
        auto next_state_do = [&state](node_type ** edge_node, size_type) {
          state = *edge_node;
        };

        std::ignore = scope->find_edge(next_state_do, key);
      }

      return state;
    }

    constexpr value_type * find_pointer(std::string_view p_pointer) noexcept
    {
      yy_util::tokenizer_first<std::string_view::value_type> tokenizer{yy_quad::make_const_span(p_pointer),
                                                                       json_detail::PathLevelSeparatorChar};

      auto state = root();
      while(!tokenizer.empty())
      {
        auto token = tokenizer.scan();
        if(state = find_level(std::string_view{token.begin(), token.end()}, state);
           nullptr == state)
        {
          break;
        }
      }

      value_type * found = nullptr;
      if(nullptr != state)
      {
        found = state->data();
      }
      return found;
    }

    constexpr node_type * root() noexcept
    {
      return m_nodes.data();
    }

  private:
    trie_vector m_nodes{};
    data_vector m_data{};
};

template<typename LabelType,
         typename ValueType>
struct pointers_config;

template<typename LabelType,
         typename ValueType>
struct scope_element;


template<typename LabelType,
         typename ValueType>
struct pointer_traits final
{
    using label_type = yy_traits::remove_cvr_t<LabelType>;
    using value_type = yy_traits::remove_cvr_t<ValueType>;
    using pointers_builder_type = yy_data::fm_flat_trie_ptr<label_type,
                                                            value_type,
                                                            json_pointer_detail::Query>;
    using size_type = typename pointers_builder_type::size_type;
    using pointers_config_type = pointers_config<label_type, value_type>;
    using scope_element_type = scope_element<label_type, value_type>;

    using query_type = typename pointers_builder_type::automaton;
};

template<typename LabelType,
         typename ValueType>
struct pointers_config final
{
    using traits = pointer_traits<LabelType, ValueType>;
    using query_type = typename traits::query_type;
    using size_type = typename traits::size_type;

    query_type pointers{};
    size_type max_depth{};
};

enum class ScopeType:uint8_t { None,
                               Doc,
                               Object,
                               Array };

template<typename LabelType,
         typename ValueType>
struct scope_element final
{
    using traits = pointer_traits<LabelType, ValueType>;
    using value_type = typename traits::value_type;
    using label_type = typename traits::label_type;
    using query_type = typename traits::query_type;
    using size_type = typename traits::size_type;

    std::string_view key;
    size_type idx{};
    query_type::node_type * state{};
    query_type::node_type * last_found{};
    ScopeType scope_type = ScopeType::None;
};

template<typename ValueType,
         typename Visitor>
class handler final
{
  public:
    using traits = pointer_traits<std::string, ValueType>;
    using value_type = typename traits::value_type;
    using label_type = typename traits::label_type;
    using scope_element_type = typename traits::scope_element_type;
    using scope_type = yy_quad::simple_vector<scope_element_type>;
    using pointers_config_type = typename traits::pointers_config_type;
    using query_type = typename traits::query_type;
    using visitor_type = yy_traits::remove_cvr_t<Visitor>;

    constexpr static std::size_t max_object_size = std::numeric_limits<std::size_t>::max();
    constexpr static std::size_t max_array_size = std::numeric_limits<std::size_t>::max();
    constexpr static std::size_t max_key_size = std::numeric_limits<std::size_t>::max();
    constexpr static std::size_t max_string_size = std::numeric_limits<std::size_t>::max();

    template<typename ...Args>
    constexpr explicit handler(pointers_config_type && p_config, Args && ...args) noexcept:
      m_scope(),
      m_pointers(std::move(p_config.pointers)),
      m_visitor(std::forward<Args>(args)...)
    {
      m_int_buffer.reserve(yy_util::Digits<typename scope_element_type::size_type>::digits + 1);
      m_scope.reserve(p_config.max_depth);
    }

    constexpr handler() noexcept = delete;
    handler(const handler &) = delete;
    constexpr handler(handler &&) noexcept = default;

    handler & operator=(const handler &) = delete;
    constexpr handler & operator=(handler &&) noexcept = default;


    constexpr value_type * find(std::string_view p_pointer) noexcept
    {
      return m_pointers.find_pointer(p_pointer);
    }

    constexpr void reset() noexcept
    {
      m_scope.clear(yy_quad::ClearAction::Keep);
    }

    constexpr bool on_document_begin(boost::json::error_code & /* ec */)
    {
      m_scope.emplace_back(scope_element_type{"", 0, m_pointers.root(), nullptr, ScopeType::Doc});
      return true;
    }

    constexpr bool on_document_end(boost::json::error_code & /* ec */)
    {
      m_scope.pop_back(yy_quad::ClearAction::Keep);
      return true;
    }

    constexpr bool on_object_begin(boost::json::error_code & /* ec */)
    {
      handle_scope();

      auto & curr = m_scope.back();

      m_scope.emplace_back(scope_element_type{"", 0, curr.last_found, nullptr, ScopeType::Object});
      return true;
    }

    constexpr bool on_object_end(std::size_t /* size */,
                                 boost::json::error_code & /* ec */)
    {
      m_scope.pop_back(yy_quad::ClearAction::Keep);
      return true;
    }

    constexpr bool on_array_begin(boost::json::error_code & /* ec */)
    {
      handle_scope();

      auto & curr = m_scope.back();

      m_scope.emplace_back(scope_element_type{"", 0, curr.last_found, nullptr, ScopeType::Array});
      return true;
    }

    constexpr bool on_array_end(std::size_t /* size */,
                                boost::json::error_code & /* ec */)
    {
      m_scope.pop_back(yy_quad::ClearAction::Keep);
      return true;
    }

    constexpr bool on_key_part(std::string_view /* part */,
                               std::size_t /* size */,
                               boost::json::error_code & /* ec */)
    {
      return true;
    }

    constexpr bool on_key(std::string_view key,
                          std::size_t /* size */,
                          boost::json::error_code & /* ec */)
    {
      auto & curr = m_scope.back();

      curr.last_found = m_pointers.find_level(key, curr.state);
      curr.key = key;

      return true;
    }

    constexpr bool on_string_part(std::string_view /* part */,
                                  std::size_t /* size */,
                                  boost::json::error_code & /* ec */)
    {
      return true;
    }

    constexpr bool on_string(std::string_view raw_str,
                             std::size_t /* size */,
                             boost::json::error_code & /* ec */)
    {
      if(handle_scope())
      {
        apply(raw_str);
      }

      return true;
    }

    constexpr bool on_number_part(std::string_view /* part */,
                                  boost::json::error_code & /* ec */)
    {
      return true;
    }

    constexpr bool on_int64(std::int64_t num,
                            std::string_view raw_str,
                            boost::json::error_code & /* ec */)
    {
      if(handle_scope())
      {
        apply(raw_str, num);
      }

      return true;
    }

    constexpr bool on_uint64(std::uint64_t num,
                             std::string_view raw_str,
                             boost::json::error_code & /* ec */)
    {
      if(handle_scope())
      {
        apply(raw_str, num);
      }

      return true;
    }

    constexpr bool on_double(double num,
                             std::string_view raw_str,
                             boost::json::error_code & /* ec */)
    {
      if(handle_scope())
      {
        apply(raw_str, num);
      }

      return true;
    }

    constexpr bool on_bool(bool flag,
                           boost::json::error_code & /* ec */)
    {
      if(handle_scope())
      {
        apply(flag);
      }

      return true;
    }

    constexpr bool on_null(boost::json::error_code & /* ec */)
    {
      handle_scope();
      return true;
    }

    constexpr bool on_comment_part(std::string_view /* part */,
                                   boost::json::error_code & /* ec */)
    {
      return true;
    }

    constexpr bool on_comment(std::string_view /* comment */,
                              boost::json::error_code & /* ec */)
    {
      handle_scope();
      return true;
    }

    visitor_type & visitor() noexcept
    {
      return m_visitor;
    }

    const visitor_type & visitor() const noexcept
    {
      return m_visitor;
    }

  private:
    static constexpr auto int_buffer_format{FMT_COMPILE("")};

    constexpr bool handle_scope() noexcept
    {
      auto & curr = m_scope.back();

      switch(curr.scope_type)
      {
        case ScopeType::Array:
        {
          m_int_buffer.clear();
          fmt::format_to(std::back_inserter(m_int_buffer), int_buffer_format, curr.idx);

          curr.last_found = m_pointers.find_level(std::string_view{m_int_buffer}, curr.state);
          ++curr.idx;
          break;
        }

        case ScopeType::Object:
          break;

        case ScopeType::Doc:
          curr.last_found = m_pointers.root();
          break;

        case ScopeType::None:
          // Do nothing.
          break;
      }

      return nullptr != curr.last_found;
    }

    constexpr value_type * get_payload() noexcept
    {
      value_type * payload = nullptr;
      auto & curr = m_scope.back();

      if(auto level = curr.last_found;
         nullptr != level && !level->empty())
      {
        payload = level->data();
      }

      return payload;
    }

    constexpr void apply(std::string_view str)
    {
      if(auto payload = get_payload();
         nullptr != payload)
      {
        m_visitor.apply_str(*payload, str);
      }
    }

    constexpr void apply(std::string_view raw, int64_t data)
    {
      if(auto payload = get_payload();
         nullptr != payload)
      {
        m_visitor.apply_int64(*payload, raw, data);
      }
    }

    constexpr void apply(std::string_view raw, uint64_t data)
    {
      if(auto payload = get_payload();
         nullptr != payload)
      {
        m_visitor.apply_uint64(*payload, raw, data);
      }
    }

    constexpr void apply(std::string_view raw, double data)
    {
      if(auto payload = get_payload();
         nullptr != payload)
      {
        m_visitor.apply_double(*payload, raw, data);
      }
    }

    constexpr void apply(bool flag)
    {
      if(auto payload = get_payload();
         nullptr != payload)
      {
        m_visitor.apply_bool(*payload, flag);
      }
    }

    std::string m_int_buffer{};
    scope_type m_scope{};
    query_type m_pointers{};
    visitor_type m_visitor{};
};

} // namespace json_pointer_detail

template<typename ValueType,
         typename Visitor>
class json_pointer_builder final
{
  public:
    using traits = json_pointer_detail::pointer_traits<std::string, ValueType>;
    using value_type = typename traits::value_type;
    using size_type = typename traits::size_type;
    using pointers_builder_type = typename traits::pointers_builder_type;
    using handler_type = json_pointer_detail::handler<value_type, yy_traits::remove_cvr_t<Visitor>>;

    using scope_element_type = typename traits::scope_element_type;
    using pointers_config_type = typename traits::pointers_config_type;

    constexpr json_pointer_builder() noexcept = default;
    json_pointer_builder(const json_pointer_builder &) = delete;
    constexpr json_pointer_builder(json_pointer_builder &&) noexcept = default;

    json_pointer_builder & operator=(const json_pointer_builder &) = delete;
    constexpr json_pointer_builder & operator=(json_pointer_builder &&) noexcept = default;

    struct data_added_type final
    {
        value_type * data = nullptr;
        bool added = false;
    };

    template<typename InputValueType>
    constexpr data_added_type add_pointer(std::string_view p_pointer, InputValueType && value)
    {
      static_assert(std::is_convertible_v<yy_traits::remove_cvr_t<InputValueType>, value_type>
                    || (std::is_pointer_v<InputValueType> && std::is_base_of_v<value_type, yy_traits::remove_cvr_t<std::remove_pointer<InputValueType>>>),
                    "Value is of an incompatible type.");

      auto levels = json_pointer_tokenize(json_pointer_trim(p_pointer));

      m_max_depth = std::max(m_max_depth + 1, levels.size());

      auto [data, added] = m_pointers_builder.add(levels, std::forward<InputValueType>(value));

      return data_added_type{data, added};
    }

    constexpr pointers_config_type create()
    {
      return create(m_max_depth);
    }

    constexpr pointers_config_type create(size_t p_max_depth)
    {
      m_max_depth = p_max_depth;

      return pointers_config_type{m_pointers_builder.create_automaton(), m_max_depth};
    }

  private:
    pointers_builder_type m_pointers_builder{};
    size_type m_max_depth{};
};

} // namespace yafiyogi:yy_json
