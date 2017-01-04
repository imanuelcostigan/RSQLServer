# The MIT License (MIT)
#
# Copyright (c) [2013-2015] [RStudio]
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
#   The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

# LICENSE above applies to this source file only
# Reason for copy, paste and mod:
# http://stackoverflow.com/questions/20515358/rcmd-check-unexported-objects-imported-by-calls#comment30669909_20515358
# https://stat.ethz.ch/pipermail/r-devel/2013-August/thread.html#67180


random_table_name <- function (n = 10, temp = FALSE) {
  name <- paste0(sample(letters, n, replace = TRUE), collapse = "")
  if (temp) prefix <- "#" else prefix <- ""
  paste0(prefix, name)
}

compact <- function(x) Filter(Negate(is.null), x)

