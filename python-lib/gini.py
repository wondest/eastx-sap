# 计算基尼系数的程序

# 按 Shift+F10 执行或将其替换为您的代码。
# 按 双击 Shift 在所有地方搜索类、文件、工具窗口、操作和设置。

import numpy as np
from itertools import groupby


#
# Calculate the gini
#
def process(total_list):
    """A Legendre series class.

    The Legendre class provides the standard Python numerical methods
    '+', '-', '*', '//', '%', 'divmod', '**', and '()' as well as the
    attributes and methods listed in the `ABCPolyBase` documentation.

    Parameters
    ----------
    total_list : array_like
        [ {"xx": aa, "yy": 1},{"xx": bb, "yy": 2} ]

        .. versionadded:: 1.6.0

    """

    model_group = groupby(total_list, key=lambda x: x["model"])

    for key, group in model_group:
        process_group(list(group))


def process_group(score_list):
    # 排序 升序
    score_sorted = sorted(score_list, key=lambda x: x["lower"])

    # 计算 total
    total_good_num = 0
    total_bad_num = 0
    for score in score_sorted:
        total_good_num = total_good_num + score["good"]
        total_bad_num = total_bad_num + score["bad"]

    # 计算
    sum_good_num = 0
    sum_bad_num = 0
    last_retain_good_pct = 0
    last_retain_bad_pct = 0
    sum_z = 0
    max_ks = 0

    for score in score_sorted:
        sum_good_num = sum_good_num + score["good"]
        sum_bad_num = sum_bad_num + score["bad"]
        retain_good_pct = sum_good_num / total_good_num
        retain_bad_pct = sum_bad_num / total_bad_num

        ## gini
        sum_z = sum_z + (retain_good_pct + last_retain_good_pct) * (retain_bad_pct - last_retain_bad_pct)
        last_retain_good_pct = retain_good_pct
        last_retain_bad_pct = retain_bad_pct

        ## ks
        max_ks = max(abs(retain_good_pct - retain_bad_pct), max_ks)

    gini = 1 - sum_z
    return [gini, max_ks]


# 按间距中的绿色按钮以运行脚本。
if __name__ == '__main__':
    input_list = [
        {"model": "Dragon", "lower": 11, "upper": 20, "good": 45000, "bad": 2000},
        {"model": "Dragon", "lower": 0, "upper": 10, "good": 5000, "bad": 2000},
        {"model": "Dragon", "lower": 21, "upper": 30, "good": 200000, "bad": 2000},
        {"model": "Wisdom", "lower": 11, "upper": 20, "good": 45000, "bad": 2000},
        {"model": "Wisdom", "lower": 0, "upper": 10, "good": 5000, "bad": 2000},
        {"model": "Wisdom", "lower": 21, "upper": 30, "good": 200000, "bad": 2000},
    ]

    process(input_list)
