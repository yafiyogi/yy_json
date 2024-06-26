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

#include <algorithm>

#include "yy_cpp/yy_string_util.h"
#include "yy_cpp/yy_tokenizer.h"

#include "yy_json_constants.h"
#include "yy_json_pointer_util.h"

namespace yafiyogi::yy_json {

std::string_view json_pointer_trim(const std::string_view p_pointer) noexcept
{
  return yy_util::trim_right(yy_util::trim(p_pointer), json_detail::PathLevelSeparator);
}

PathLevels json_pointer_tokenize(const std::string_view p_pointer) noexcept
{
  std::size_t num_levels = static_cast<std::size_t>(std::count(p_pointer.begin(), p_pointer.end(), json_detail::PathLevelSeparatorChar));
  PathLevels levels;
  levels.reserve(num_levels);

  yy_util::tokenizer<std::string_view::value_type> tokenizer{yy_quad::make_const_span(p_pointer),
                                                             json_detail::PathLevelSeparatorChar};
  while(!tokenizer.empty())
  {
    auto level = tokenizer.scan();
    levels.emplace_back(PathLevel{level.begin(), level.end()});
  }

  return levels;
}

} // namespace yafiyogi::yy_json
