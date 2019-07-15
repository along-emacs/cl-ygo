-*- coding: utf-8-unix -*-
# cl-ygo
Common Lisp 版本的 YGO 。 \
代码更新中，此文档仅供参考，实际情况以代码为准。

## 组织
目录结构与项目：[ygopro](https://github.com/Fluorohydride/ygopro) 一致，因为需要引用其 `cards.cdb` 与卡牌的图片。
自底向上，先完成基础的框架。从 `ydk` 文件开始，初始化一个卡组，随后是对卡片的一系列基础操作，接下来是效果的存储与发动，之后再考虑其他的。

## 功能
1. 在指定的位置根据名字检索卡牌
2. TODO: 使用 `lisp` 风格的，且尽量接近自然语言的方式处理更多的检索条件。\
   比如要检索 `8星以下的龙族或魔法师族通常怪兽` 的代码如下：
   ```commonlisp
   (search-cards (and (or (race = :dragon)
                          (race = :spellcaster))
                      (level <= 8)
                      (type = :normal)))
   ```
3. TODO: 实现卡牌从一个区域移动到另一个区域。 \
   常见的有 `抽卡` 这个操作，比如：
   ```commomlisp
   (move-cards :deck :hand '(#<card1> #<card2>))
   <==>
   (draw 2)
   ```
4. TODO: 其他。

## 代码说明
### 变量
变量的命名规则：
1. 体现出单复数
2. 补充 `1` ，当第二个变量能够体现前一个变量的复数状态时，前一个变量需要使用单数
3. 全局变量两边加 `*`
4. 变量名与变量名之间使用 `-` 连接

#### \*deck-dir\*
存放卡组 `ydk` 文件的目录名。

#### \*cards-db\*
定义卡牌信息数据库的文件名。目前暂时设置的和 `ygopro` 一样。

#### \*cards-index\*
为在初始化卡组时所有创建的 `card` 对象的实例添加一次引用。

#### \*zones-list\*
定义了所有区域的列表，每个区域名字用关键字表示。

#### \*card-lists\*
根据区域列表生产空的列表，用来存放 `card` 对象实例。

### 函数
#### empty-index ()
将 `*cards-index*` 指向 `nil` ，这将减少卡牌对象的一次引用。

#### empty-deck ()
遍历 `*card-lists` ，将每一项指向 `nil` 。

#### decks (&rest zones)
可传入 0-n 个参数，默认为 `deck` 。
返回的结果会带上区域名：
```commomlisp
(decks :side :hand) =>
'(:side (#<card1> #<card2>)
  :extra (#<card3>))
```

#### cards (&rest zones)
可传入 0-n 个参数，默认为 `deck` 。
返回的结构不会带上区域名：
```commomlisp
(cards :side :hand) =>
'(#<card1> #<card2> #<card3>)
```

#### runsql (sql)
前期测试用。

#### get-dir-of (&rest paths)
获取完整目录。

#### get-card-by-id (id)
用于从 `cards.cdb` 中读取信息并生成 `card` 的实例返回。
TODO: 完善获取信息的 `sql` 语句。

#### parse-deck (name)
解析 `ydk` 卡组文件。
假设 `example.ydk` 文件的格式如下：
```text
#created by ...
#main
card-id-1
card-id-2
#extra
card-id-3
card-id-4
!side
card-id-5
```
将返回如下列表：
```commonlisp
(parse-deck "example") =>
'(:deck (card-id-1 card-id-2 )
 :extra (card-id-3 card-id-4) :side (card-id-5))
```

#### fill-deck (id &optional (zone :deck))
根据卡牌 `id` 创建对象实例，并加入到指定的区域列表中。

#### init-deck (name)
通过传入的 `ydk` 文件名初始化卡组。
会将原有的 `*cards-index*` 和 `*card-lists*` 清空。

#### search-cards-by-name (name &rest zones)
通过卡牌名，在指定的区域中查找。可传入多个区域。
将会返回区域名：
```commonlisp
(search-cards-by-name "混沌") =>
'(:DECK (#<青眼混沌极龙>  #<青眼混沌极龙>  #<青眼混沌龙>  #<混沌形态>  #<混沌形态> ))

(search-cards-by-name "No" :extra :hand) =>
'(:EXTRA
  (#<No.107 银河眼时空龙>  #<No.90 银河眼光子卿>
   #<闪光No.39 希望皇 霍普·电光皇>  #<No.39 希望皇 霍普> )
  :HAND NIL)
```

#### search-cards-by-sequence (number &rest zones)
返回指定区域的前几张卡牌。可传入多个区域：
```commonlisp
(search-cards-by-sequence 2 :extra :deck) =>
'(:EXTRA (#<青眼双爆裂龙>  #<始祖龙 古龙> ) :DECK (#<青眼白龙>  #<青眼白龙> ))

(search-cards-by-sequence 2 :hand :side) =>
'(:HAND NIL :SIDE (#<No.98 绝望皇 霍普勒斯>  #<刺刀枪管龙> ))
```
