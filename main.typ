#import "@preview/ilm:1.4.0": *
#import "@preview/mannot:0.2.1": *

#import "@preview/codly:1.2.0": *
#import "@preview/codly-languages:0.1.1": *
#show: codly-init
#codly(languages: codly-languages)

#import "@preview/fletcher:0.5.4" as fletcher: node
#let diagram = fletcher.diagram.with(node-stroke: .1em)
#let trans = fletcher.edge.with(marks: "-|>")
#let state = fletcher.node.with(shape: circle)
#let accept_state = fletcher.node.with(extrude: (-2.5, 0))

#set text(lang: "zh")

#show: ilm.with(
  title: "计算理论导论笔记",
  author: "ShenMian",
  abstract: [
    本文为计算理论导论 (#link("https://ocw.mit.edu/courses/18-404j-theory-of-computation-fall-2020/")[MIT18.404J], #link("https://ocw.mit.edu/")[MIT OCW]) 的课程笔记.
  ],
  preface: [
    #align(center + horizon)[
      本文采用 #link("https://creativecommons.org/licenses/by-nc-sa/4.0/")[CC BY-NC-SA 4.0] 许可协议进行授权.
      #image("img/cc-by-nc-sa.svg", width: 15%)
    ]
  ],
  bibliography: bibliography("refs.bib"),
  figure-index: (enabled: true, title: "图片索引"),
  table-index: (enabled: false, title: "表格索引"),
  listing-index: (enabled: true, title: "代码索引")
)

#set text(font: ("New Computer Modern", "Source Han Serif SC"))
#show raw: set text(font: "Cascadia Code NF")

#show figure.where(kind: raw): set block(breakable: true)

// FIXME: 无法为公式添加标签

= 自动机理论

*自动机 (automata)* 一词来源于希腊单词 #text(lang: "el")[αὐτόματος], 意为自动 (self-acting).

计算机十分复杂, 研究其计算过程时通常采用一种简化的抽象模型, 即自动机. \
这种简化是通过剔除计算机中与计算无关的特征来实现的, 最终得到一个计算模型 (computational model).

#figure(
  image("img/automata_theory.svg", width: 50%),
  caption: [自动机理论. @wikipedia_automata_theory],
) <automata-theory>

@automata-theory 按照计算能力的强弱, 对三种自动机进行了分类:
- *有限状态自动机 (finite-state machine, FSM)*: 计算能力最低的自动机.
  - *确定有限自动机*: 将在 @dfa 中介绍.
  - *非确定有限自动机*: 将在 @nfa 中介绍.
- *下推自动机*: 将在 @pda 中介绍.
- *图灵机*: 计算能力与现代计算机等价, 将在 @tm 中介绍.

== 确定有限自动机 (Deterministic finite automata) <dfa>

确定有限自动机 (以下简称 DFA) 是一种自动机, 其中 "确定" 和 "有限" 是对该自动机的限制, 含义如下:
- *确定*: 此处的 "确定" 是相对于*非确定*而言的. 后续将在 @nfa 介绍非确定有限自动机, 届时将说明确定和非确定的含义.
- *有限*: DFA 也被称为确定*有限状态*自动机, 顾名思义, 其中的 "有限" 指*状态的数量有限*.

下图是 DFA $M_1$ 的状态图:

#figure(
  caption: [$M_1$ 的状态图.],
  diagram(
    spacing: 2em,
    label-sep: -0.1em,

    trans((-1, 0), "r", [Start], label-pos: 0),

    state((0, 0), name: <q1>, [$q_1$]),
    accept_state((1, 0), name: <q2>, [$q_2$]),
    accept_state((2, 0), name: <q3>, [$q_3$]),

    trans(<q1>, <q1>, [0], bend: 130deg),
    trans(<q1>, <q2>, [1], bend: 30deg),

    trans(<q2>, <q1>, [0], bend: 30deg),
    trans(<q2>, <q3>, [1]),

    trans(<q3>, <q3>, [0,1], bend: 130deg),
  )
)

- *输入*: *有限*的字符串.
- *输出*: *接受 (accept)* 或*拒绝 (reject)*.
- *计算过程*: 从起始状态开始, 读取输入符号, 进行对应的*转移 (transition)*, 最终处于接受状态则接受, 否则拒绝 (没有对应的转移则直接拒绝).

以 $M_1$ 为例, 当输入的字符串为:
- "01101": 结束状态为 $q_3$, 属于接受状态, 所以接受.
- "00101": 结束状态为 $q_2$, 不属于接受状态, 所以拒绝.

分析后可知, $M_1$ 能接受任意包含子串 "11" 的字符串. \
所有能被机器识别的字符串集合被称为该机器的*语言 (language)*.

$M_1$ 的语言 $A$ 可以表示为:

$
A &= L(M_1) \
  &= {w | w "包含子串 \"11\""}
$

=== 正式定义

一个 DFA $M$ 是一个五元组 $(Q, Sigma, delta, q_0, F)$, 其中:
- $Q$ 是*状态 (state)* 的有限集.
- $Sigma$ 是由*字母表 (alphabet)*#footnote[此处的字母表是形式语言的术语, 而非拉丁字母表. 是由不可分割的符号/字符/字形组成的非空集合.] 符号组成的有限集.
- $delta: Q times Sigma -> Q$ 是*转移函数*.
- $q_0 in Q$ 是*初始状态 (start state)*.
- $F subset.eq Q$ 是*接受状态 (accept states)* 的集合.

$M_1$ 的符号化表示如下:

$
  M_1 &= (Q, Sigma, delta, q_0, F) \
    Q &= {q_1, q_2, q_3} \
Sigma &= {0, 1} \
  q_0 &= q_1 \
    F &= {q_3} \
delta &=
  mat(
      ,    0,   1;
    q_1, q_1, q_2;
    q_2, q_1, q_3;
    q_3, q_3, q_3;
    delim: #none,
    augment: #(vline: 1, hline: 1)
  )
$

其中转移函数可以使用状态转移表来表示:

#figure(caption: [$M_1$ 的状态转移表])[
  #table(
    columns: 3,
    table.header[][*0*][*1*],
    [*$q_1$*], [$q_1$], [$q_2$],
    [*$q_2$*], [$q_1$], [$q_3$],
    [*$q_3$*], [$q_3$], [$q_3$],
  )
]

表格中的横轴表示输入符号, 纵轴表示当前状态, 表中对应的值表示下一个状态.

=== 计算过程

- *字母表*: 由符号构成的有限集.
- *字符串 (string)*: 由 $Sigma$ 中符号构成的有限序列.
- *语言*: 字符串的集合 (有限或无限).
- *空字符串*: 长度为 0 的字符串, 写作 $epsilon$.
- *空语言*: 不包含任何字符串的集合, 即空集. 写作 $emptyset$.

*定义*: $M$ 接受字符串 $w = w_1 w_2 ... w_3, "each" w_i in Sigma$,
如果存在状态序列: $r_0, r_1, ..., r_n, r_i in Q$, 满足以下条件:
- $r_0 = q_0$.
- $r_i = delta(r_(i-1), w_i) "for" 1 <= i <= n$.
- $r_n in F$.

=== 代码实现

DFA 的 Rust 代码实现为:

#figure(caption: [确定有限自动机.])[
```rust
use std::marker::PhantomData;

struct Dfa<S, A, D>
where
    S: Copy + PartialEq,
    D: Fn(S, &A) -> S,
{
    start_state: S,
    accept_states: Vec<S>,
    transition: D,
    _phantom_data: PhantomData<A>,
}

impl<S, A, D> Dfa<S, A, D>
where
    S: Copy + PartialEq,
    D: Fn(S, &A) -> S,
{
    fn new(start_state: S, accept_states: Vec<S>, transition: D) -> Self {
        Self {
            start_state,
            accept_states,
            transition,
            _phantom_data: PhantomData,
        }
    }

    fn recognize(&self, string: &[A]) -> bool {
        let mut state = self.start_state;
        for input in string {
            state = (self.transition)(state, input);
        }
        self.accept_states.contains(&state)
    }
}
```
]

#figure(caption: [确定有限自动机 $M_1$.])[
```rust
#[derive(Clone, Copy, PartialEq)]
enum State {
    Q1,
    Q2,
    Q3,
}

enum Alphabet {
    Zero,
    One,
}

fn main() {
    let m1 = Dfa::new(
        State::Q1,
        vec![State::Q3],
        |state, symbol| -> State {
            match (state, symbol) {
                (State::Q1, Alphabet::Zero) => State::Q1,
                (State::Q1, Alphabet::One) => State::Q2,
                (State::Q2, Alphabet::Zero) => State::Q1,
                (State::Q2, Alphabet::One) => State::Q3,
                (State::Q3, _) => State::Q3,
            }
        }
    );

    assert!(m1.recognize(&[
        Alphabet::Zero,
        Alphabet::One,
        Alphabet::One,
        Alphabet::Zero,
        Alphabet::One
    ]));
    assert!(!m1.recognize(&[
        Alphabet::Zero,
        Alphabet::Zero,
        Alphabet::One,
        Alphabet::Zero,
        Alphabet::One
    ]));
}
```
]

== 正则表达式 (Regular expressions)

*正则表达式*由 $Sigma$, $emptyset$, $epsilon$ 构成, 并通过正则操作组合得到. 用于定义正则语言.

例如:
- $Sigma^*$1: 任意以 "1" 结尾的字符串.
- $Sigma^*$11$Sigma^*$: 任意包含子串 "11" 的字符串, 即 $L(M_1)$.

=== 正则语言 (Regular language)

*正则语言*是一种由正则表达式定义的形式语言.

=== 正则操作 (Regular operations)

*正则操作*是针对语言的运算符.

令 A, B 为语言:
- *并 (Union)*: $A union B$.
- *连接 (Concatenation)*: $A compose B = {x y | x in A and y in B} = A B$.
- *Kleene 星 (Kleene star)*: $A^* = {x_1 ... x_k | "each" x_i in A "for" k >= 0}$.

令 $A = {0, 1}$, $B = {2, 3}$:
- $A union B = {0, 1, 2, 3}$.
- $A compose B = {02, 03, 12, 13}$.
- $A^* = {epsilon, 0, 1, 01, 10, 11, 00, 001, ...}$ (除非 $A$ 是空集, 否则 $A^*$ 是无限集).

=== 闭包属性 (Closure properties)

此处的*闭包*是数学中的术语, 而非计算机科学中的闭包概念.

#blockquote[
  若对某个集合的成员进行一种运算, 生成的仍然是这个集合的成员, 则该集合被称为在这个运算下闭合. -- Wikipedia
]

比如自然数在加法运算下闭合, 因为两个自然数相加所得到的结果一定是自然数. \
但是自然数在减法运算下不闭合, 因为两个自然数相减可能得到非自然数的结果, 如负整数.

正则语言有以下闭包属性:
- *定理 1*: 如果 $A_1$, $A_2$ 是正则语言, 则 $A_1 union A_2$ 也是正则语言 (即正则语言在联合运算下闭合).
- *定理 2*: 如果 $A_1$, $A_2$ 是正则语言, 则 $A_1 compose A_2$ 也是正则语言.
- *定理 3*: 如果 $A_1$, $A_2$ 是正则语言, 则 $A^*$ 也是正则语言.

==== 证明定理 1

*证明*: 若 $M_1$ 识别 $A_1$, $M_2$ 识别 $A_2$, 则有一台 DFA $M$ 能识别 $A_1 union A_2$.

有以下两种方案:
- *分别运行 $M_1$ 和 $M_2$*: DFA 无法直接实现这种操作, 因为 DFA 的运行 (即状态的转移) 需要*消耗字符串中的字符*, 因此无法多次*尝试*同一个字符串.
- *合并 $M_1$ 和 $M_2$*: 使合并后的 $M$ 可以同时做 $M_1$ 和 $M_2$ 的工作. 在转移函数里, 输入的字符可以被多次使用.

因此可行的方法是将两台自动机合并, 使他们并行地处理一个字符串.

这意味着自动机 $M$ 能在单个状态里同时表示 $M_1$ 和 $M_2$ 的状态, 如下所示:

$
Q = Q_1 times Q_2 = { (q_1, q_2) | q_1 in Q_1 and q_2 in Q_2 } \
q_0 = (q_1, q_2)
$

这样 $M$ 就可以并行地跟踪 $M_1$ 和 $M_2$ 的状态.

转移函数也需要并行地进行状态转移:

$ delta((q, r), a) = (delta_1(q, a), delta_2(r, a)) $

只要 $q in Q_1$ 或 $r in Q_2$, 该状态即为 $M$ 的接受状态:

$
F = mark((F_1 times Q_2), tag: #<f1>) union mark((Q_1 times F_2), tag: #<f2>, color: #blue)

#annot(<f1>, pos: top)[$M_1$ 的接受状态与 $M_2$ 的任意状态]
#annot(<f2>, pos: bottom)[$M_1$ 的任意状态与 $M_2$ 的接受状态]
$

其中*定理 2* 和*定理 3*无法通过简单的构造 DFA 来证明, @nfa 将介绍非确定有限自动机, 该自动机与 DFA 等价. @nfa-to-dfa 将利用非确定有限自动机简化上述*定理 1* 的证明, 并证明另外两种正则操作的闭合属性.

== 非确定有限自动机 (Nondeterministic finite automata) <nfa>

*非确定*一词来源于计算机科学术语 "非确定性算法"@floyd_1967. 非确定性算法用于简化回溯算法的设计.

下面是非确定有限自动机 (以下简称 NFA) $N_1$ 的状态图:

#figure(
  caption: [$N_1$ 的状态图.],
  diagram(
    spacing: 2em,
    label-sep: -0.1em,

    trans((-1, 0), "r", [Start], label-pos: 0),

    state((0, 0), name: <q1>, [$q_1$]),
    state((1, 0), name: <q2>, [$q_2$]),
    state((2, 0), name: <q3>, [$q_3$]),
    accept_state((3, 0), name: <q4>, [$q_4$], extrude: (-2.5, 0)),

    trans(<q1>, <q1>, [a], bend: 130deg),
    trans(<q1>, <q2>, [a], bend: 30deg),

    trans(<q2>, <q1>, [b], bend: 30deg),
    trans(<q2>, <q3>, [b]),

    trans(<q3>, <q4>, [$a$]),
  )
)

对于 DFA 来说, 转移由源状态和输入符号确定, 并且唯一. \
而 NFA 中转移则不一定唯一, 以 $N_1$ 为例:
- $q_1$ 对于输入 `a` 可以转移到 $q_1$ 或 $q_2$.
- $q_2$ 对于输入 `b` 可以转移到 $q_1$ 或 $q_3$.

若输入可以通过任意转移方式最终转移到接受状态则接受, 否则拒绝.

=== $epsilon$-NFA

与 NFA 相比 $epsilon$-NFA (以下简称为 eNFA) 支持 $epsilon$-转移.

下面是 eNFA $N_2$ 的状态图:

#figure(
  caption: [$N_2$ 的状态图.],
  diagram(
    spacing: 2em,
    label-sep: -0.1em,

    trans((-1, 0), "r", [Start], label-pos: 0),

    state((0, 0), name: <q1>, [$q_1$]),
    state((1, 0), name: <q2>, [$q_2$]),
    state((2, 0), name: <q3>, [$q_3$]),
    accept_state((3, 0), name: <q4>, [$q_4$], extrude: (-2.5, 0)),

    trans(<q1>, <q1>, [a], bend: 130deg),
    trans(<q1>, <q2>, [a], bend: 30deg),

    trans(<q2>, <q1>, [b], bend: 30deg),
    trans(<q2>, <q3>, [b]),

    trans(<q3>, <q4>, [$a, epsilon$]),
  )
)

以 $N_2$ 为例, 当输入的字符串为:
- "ab": 结束状态可能为 $q_1$, $q_3$, $q_4$, 其中 $q_4$ 属于接受状态, 所以接受.
- "aa": 结束状态可能为 $q_1$, $q_2$, 均不属于接受状态, 所以拒绝.
- "abb": 结束状态只可能为 $q_1$, 不属于接受状态, 所以拒绝.

=== 正式定义

==== NFA

NFA 的新特性*均与转移有关*, 因此它与 DFA 的五元组定义相似, 唯一的区别在于转移函数:

$
delta: Q times Sigma -> mark(cal(P)(Q), tag: #<powerset>) = { R | R subset.eq Q }

#annot(<powerset>, pos: bottom)[$Q$ 的幂集]
$

#figure(caption: [$N_1$ 的状态转移表])[
  #table(
    columns: 3,
    table.header[][`a`][`b`],
    [*$q_1$*], [${q_1, q_2}$], [$emptyset$],
    [*$q_2$*], [$emptyset$], [${q_1, q_3}$],
    [*$q_3$*], [{$q_4$}], [$emptyset$],
    [*$q_4$*], [$emptyset$], [$emptyset$],
  )
]

==== eNFA

eNFA 的定义与 NFA 类似, 但转移函数允许 $epsilon$-转移:

$
delta: Q times mark(Sigma_epsilon, tag: #<se>) -> cal(P)(Q) = { R | R subset.eq Q }

#annot(<se>, pos: bottom)[${Sigma union epsilon}$]
$

允许 $epsilon$ (即空字符串) 作为转移标签, 即可以在不消耗任何输入的情况下进行转移.

#figure(caption: [$N_2$ 的状态转移表])[
  #table(
    columns: 4,
    table.header[][`a`][`b`][*$epsilon$*],
    [*$q_1$*], [${q_1, q_2}$], [$emptyset$], [$emptyset$],
    [*$q_2$*], [$emptyset$], [${q_1, q_3}$], [$emptyset$],
    [*$q_3$*], [{$q_4$}], [$emptyset$], [{$q_4$}],
    [*$q_4$*], [$emptyset$], [$emptyset$], [$emptyset$],
  )
]

以 $N_2$ 为例:
- $delta(q_1, a) = {q_1, q_2}$.
- $delta(q_1, b) = emptyset$.

=== 代码实现

eNFA 的 Rust 代码实现为:

#figure(caption: [非确定有限自动机.])[
```rust
use std::marker::PhantomData;

struct Nfa<S, A, D>
where
    S: Copy + PartialEq + 'static,
    D: Fn(S, Option<&A>) -> &'static [S],
{
    start_state: S,
    accept_states: Vec<S>,
    transition: D,
    _phantom_data: PhantomData<A>,
}

impl<S, A, D> Nfa<S, A, D>
where
    S: Copy + PartialEq + 'static,
    D: Fn(S, Option<&A>) -> &'static [S],
{
    fn new(start_state: S, accept_states: Vec<S>, transition: D) -> Self {
        Self {
            start_state,
            accept_states,
            transition,
            _phantom_data: PhantomData,
        }
    }

    fn recognize(&self, string: &[A]) -> bool {
        self.recognize_inner(self.start_state, string)
    }

    fn recognize_inner(&self, state: S, string: &[A]) -> bool {
        match string {
            [] => {
                self.accept_states.contains(&state)
                    || (self.transition)(state, None)
                        .iter()
                        .any(|state| self.accept_states.contains(state))
            }
            [symbol, rest @ ..] => {
                (self.transition)(state, Some(symbol))
                    .iter()
                    .any(|&state| self.recognize_inner(state, rest))
                    || (self.transition)(state, None)
                        .iter()
                        .any(|&state| self.recognize_inner(state, string))
            }
        }
    }
}
```
]

#figure(caption: [非确定有限自动机 $N_2$.])[
```rust
#[derive(Clone, Copy, PartialEq)]
enum State {
    Q1,
    Q2,
    Q3,
    Q4,
}

enum Alphabet {
    A,
    B,
}

fn main() {
    let n1 = Nfa::new(State::Q1, vec![State::Q4], |state, symbol| {
        match (state, symbol) {
            (State::Q1, Some(Alphabet::A)) => &[State::Q1, State::Q2],
            (State::Q2, Some(Alphabet::B)) => &[State::Q1, State::Q3],
            (State::Q3, Some(Alphabet::A) | None) => &[State::Q4],
            _ => &[],
        }
    });

    assert!(n1.recognize(&[Alphabet::A, Alphabet::B]));
    assert!(!n1.recognize(&[Alphabet::A, Alphabet::A]));
    assert!(!n1.recognize(&[Alphabet::A, Alphabet::B, Alphabet::B]));
}
```
]

== NFA 转 DFA <nfa-to-dfa>

NFA 可以转化为等价的 DFA, 所谓等价是指两者能够识别相同的语言.

*定理*: 如果 NFA 能识别 $A$, 则 $A$ 是正则语言.

*证明*: 有 NFA $M = (Q, Sigma, delta, q_0, F)$ 识别 $A$, 构造 DFA $M' = (Q', Sigma, delta', q'_0, F')$ 也识别 $A$.

$M'$ 中的状态应该能表示 $M$ 中任何状态所构成的合集, 所以 $M'$ 的状态就是 $M$ 中状态的幂集:

$ Q' = cal(P)(Q) $

$M'$ 中的状态 $R$, 用于同时跟踪 $M$ 中的多个状态. \
因此在进行状态转移的时候, 也需要分别转移 $R$ 中的每个状态:

$
delta'(mark(R, tag: #<r>), a) = { q | q in delta(r, a) "for some" r in R}

#annot(<r>, pos: bottom, yshift: 0.5em)[$R in Q'$]
$

NFA 和 DFA 都只有一个起始状态, 但 $M'$ 中的状态是一个集合:

$ q'_0 = {q_0} $

$ F' = {R in Q' | R "intersects" F} $

因为 NFA 可以转换为 DFA, 所以 NFA 能识别的语言也是正则语言. \
这样便可以利用 NFA 来证明正则语言相关的定理.

=== 证明定理 1

*定理 1*: 如果 $A_1$, $A_2$ 是正则语言, 则 $A_1 union A_2$ 也是正则语言.

*证明*: 若 $M_1$ 识别 $A_1$, $M_2$ 识别 $A_2$, 则有一台 NFA $M$ 能识别 $A_1 union A_2$.

也就是说, 如果 $A_1$, $A_2$ 可以被有限自动机识别, 则 $A_1 union A_2$ 也可以被有限自动机识别.

NFA 的不确定性使其具有*尝试*或*猜测*的能力, 因此现在可以通过 NFA 的 $epsilon$-转移特性, 分别尝试 $A$ 能否被 $M_1$ 或 $M_2$ 所识别.

#figure(
  caption: [NFA 实现联合操作.],
  diagram(
    spacing: 2em,
    label-sep: -0.1em,

    trans((-1, 0), "r", [$M$ Start], label-pos: 0),
    state((0, 0), name: <m_q1>, [$q_1$]),
    trans(<m_q1>, <m1_q2>, [$epsilon$]),
    trans(<m_q1>, <m2_q4>, [$epsilon$]),

    trans((0, -1), "r", [$M_1$ Start], label-pos: 0),
    state((1, -1), name: <m1_q2>, [$q_2$]),
    accept_state((2, -1), name: <m1_q3>, [$q_3$]),
    trans(<m1_q2>, <m1_q3>, "wave"),

    trans((0, 1), "r", [$M_2$ Start], label-pos: 0),
    state((1, 1), name: <m2_q4>, [$q_4$]),
    accept_state((2, 1), name: <m2_q5>, [$q_5$]),
    trans(<m2_q4>, <m2_q5>, "wave"),

    node(enclose: (<m1_q2>, <m1_q3>), stroke: (dash: "dashed"), snap: false),
    node(enclose: (<m2_q4>, <m2_q5>), stroke: (dash: "dashed"), snap: false),
  )
)

=== 证明定理 2

*定理 2*: 如果 $A_1$, $A_2$ 是正则语言, 则 $A_1 compose A_2$ 也是正则语言.

*证明*: 若 $M_1$ 识别 $A_1$, $M_2$ 识别 $A_2$, 则有一台 NFA $M_3$ 能识别 $A_1 compose A_2$.

$M_3$ 接受输入 $w$, 如果 $w = x y$, 其中 $x$ 被 $M_1$ 所识别, $y$ 被 $M_2$ 所识别.

使用 DFA 证明的难点在于, 难以判断 $x$ 与 $y$ 之间的分割点. \
而 NFA 可以反复尝试, 直到找到合适的分割点.

#figure(
  caption: [NFA 实现连接操作.],
  diagram(
    spacing: 2em,
    label-sep: -0.1em,

    trans((-1, 0), "r", [$M$, $M_1$ Start], label-pos: -1),
    state((0, 0), name: <m1_q1>, [$q_1$]),
    accept_state((2, -1), name: <m1_q2>, [$q_2$]),
    accept_state((2, 1), name: <m1_q3>, [$q_3$]),
    trans(<m1_q1>, <m1_q2>, "wave"),
    trans(<m1_q1>, <m1_q3>, "wave"),

    trans((5, 0), "r", [$M_2$ Start], label-pos: -1),
    state((6, 0), name: <m2_q4>, [$q_4$]),
    accept_state((8, -1), name: <m2_q5>, [$q_5$]),
    accept_state((8, 1), name: <m2_q6>, [$q_6$]),
    trans(<m2_q4>, <m2_q5>, "wave"),
    trans(<m2_q4>, <m2_q6>, "wave"),

    trans(<m1_q2>, <m2_q4>, [$epsilon$]),
    trans(<m1_q3>, <m2_q4>, [$epsilon$]),

    node(enclose: (<m1_q1>, <m1_q2>, <m1_q3>), stroke: (dash: "dashed"), snap: false),
    node(enclose: (<m2_q4>, <m2_q5>, <m2_q6>), stroke: (dash: "dashed"), snap: false),
  )
)

=== 证明定理 3

*定理 3*: 如果 $A_1$, $A_2$ 是正则语言, 则 $A^*$ 也是正则语言.

*证明*: 若 $M$ 识别 $A_1$, 则有一台 NFA $M'$ 能识别 $A^*$.

$M'$ 接受输入 $w$, 如果 $w = x_0 x_1 ... x_k$, 其中 $k >= 0$, 任意 $x$ 被 $M$ 所识别.

与证明*定理 2* 类似, 只是现在 $w$ 可能存在多个分割点, 需要猜测这些分割点的位置, 寻找是否存在满足条件的切割点组合.

#figure(
  caption: [NFA 实现 Kleene 星操作.],
  diagram(
    spacing: 2em,
    label-sep: -0.1em,

    trans((-1, 0), "r", [$M'$ Start], label-pos: 0),
    accept_state((0, 0), name: <mp_q1>, [$q_1$]),
    trans(<mp_q1>, <m_q2>, [$epsilon$]),

    state((1, 0), name: <m_q2>, [$q_2$]),
    accept_state((3, -1), name: <m_q3>, [$q_3$]),
    accept_state((3, 1), name: <m_q4>, [$q_4$]),
    trans(<m_q2>, <m_q3>, "wave", bend: -20deg),
    trans(<m_q2>, <m_q4>, "wave", bend: 20deg),
    trans(<m_q3>, <m_q2>, [$epsilon$], bend: -20deg),
    trans(<m_q4>, <m_q2>, [$epsilon$], bend: 20deg),

    node(enclose: (<m_q2>, <m_q3>, <m_q4>), stroke: (dash: "dashed"), snap: false),
  )
)

因为 $A^*$ 一定包含 $epsilon$, 所以添加新的接受状态 $q_1$, 以确保 $M'$ 能接受空字符串.

== 正则表达式转 NFA

*证明*: 将 $R$ 转换为等效的 NFA $M$.

转换可以分为以下步骤:

+ 将正则表达式中的 $Sigma$, $emptyset$, $epsilon$ 转换为 NFA:

  #figure(
    image("img/regular_atomic_to_nfa.png", width: 60%),
    caption: [NFA 实现 Kleene 星操作. @mitocw_theory_of_computation],
  )

+ 使用 @nfa-to-dfa 提供的方法, 利用正则运算合并 NFA.

下面将以正则表达式 $(a union a b)^*$ 转换为等效的 NFA 为例:

#figure(
  image("img/regex_to_nfa.png", width: 50%),
  caption: [正则表达式转 NFA. @mitocw_theory_of_computation],
)

== DFA 转正则表达式

=== 广义非确定自动机 (Generalized NFA)

广义非确定自动机 (以下简称 GNFA), 与 NFA 相似, 且允许正则表达式作为转移标签.

#figure(
  caption: [伪广义非确定自动机状态图.],
  diagram(
    spacing: 6em,
    label-sep: -0.1em,

    // trans((-1, 0), "r", `Start`, label-pos: 0, label-side: center),

    state((0, 0), name: <q1>, [$q_1$], radius: 1em),
    accept_state((1, 0), name: <q2>, [$q_2$], radius: 1em, extrude: (-2.5, 0)),
    accept_state((0.5, calc.sin(60deg)), name: <q3>, [$q_3$], radius: 1em, extrude: (-2.5, 0)),

    trans(<q1>, <q1>, [$a$], bend: 130deg),
    trans(<q1>, <q2>, [$a^*b^*$], bend: 20deg),
    trans(<q1>, <q3>, [$a union b$], bend: -20deg),

    trans(<q2>, <q1>, [$b$], bend: 20deg, label-side: right),
    trans(<q2>, <q3>, [$a a b$], bend: 20deg),
  )
) <fake-gnfa>

==== 正式定义

一个 GNFA $G$ 是一个五元组 $(Q, Sigma, delta, s, a)$, 其中:
- $Q$ 是状态的有限集.
- $Sigma$ 是由字母表符号组成的有限集.
- $T: (S without {a}) times (S without {s}) -> R$ 是转移函数.
- $s in S$ 是初始状态.
- $a in S$ 是接受状态.

根据定义对 @fake-gnfa 进行修改后, 即可得到符合定义的 GNFA:

#figure(
  caption: [广义非确定自动机状态图.],
  diagram(
    spacing: 6em,
    label-sep: -0.1em,

    // trans((-1, 0), "r", `Start`, label-pos: 0, label-side: center),

    node((0, 0), name: <q1>, [$q_1$], radius: 1em),
    node((1, 0), name: <q2>, [$q_2$], radius: 1em),
    node((0.5, calc.sin(60deg)), name: <q3>, [$q_3$], radius: 1em),
    node((1.5, calc.sin(60deg)), name: <q4>, [$q_4$], radius: 1em, extrude: (-2.5, 0)),

    trans(<q1>, <q1>, [$a$], bend: 130deg),
    trans(<q1>, <q2>, [$a^*b^*$], bend: 20deg),
    trans(<q1>, <q3>, [$a union b$], bend: -20deg),

    trans(<q2>, <q1>, [$b$], bend: 20deg, label-side: right),
    trans(<q2>, <q3>, [$a a b$], bend: 20deg),

    trans(<q3>, <q2>, [$emptyset$], bend: 20deg),

    trans(<q2>, <q4>, [$epsilon$], label-side: right),
    trans(<q3>, <q4>, [$epsilon$], label-side: right),
  )
)

*引理*: 任意 GNFA $G$ 都有对应的正则表达式 $R$.

下面将使用*归纳法*进行证明, 即通过证明基础情况, 来证明所以可以被归纳为基础情况的情况.

下面使用 $G_k$ 表示有 $k$ 种状态的 GNFA, 需要证明:

- $G_2$ 有对应的正则表达式.
- 当 $k > 2$ 时, $G_k$ 可以转换为等效的 $G_(k - 1)$.

这样对于任何 $k > 2$ 的 $G_k$ 来说, 最终能被一个 $G_2$ 所表示.  
又因为 $G_2$ 已被证明有对应的正则表达式, 所以任意 GNFA 都有对应的正则表达式.

#figure(
  image("img/gnfa_basis.png", width: 20%),
  caption: [$G_2$ 状态图. @mitocw_theory_of_computation],
)

可以看出, $G_2$ 有对应的正则表达式 $r$.

$G_k$ 可以转换为等效的 $G_(k - 1)$ 意味着从 $G_k$ 中删除一个状态, 然后再做一些修改, 使其语言不变. 如下图所示:

#figure(
  image("img/k_to_k-1_gnfa.png", width: 80%),
  caption: [$G_(k - 1)$ 到 $G_k$ 示意图. @mitocw_theory_of_computation],
)

此处删除了一个状态, 并添加了等效的正则表达式.  
重复该过程, 就可以将 $G_k$ 转化为 $G_2$, 并最终获取能表达整个 GNFA 的正则表达式.

== 非正则语言 (Non-regular languages)

=== 泵引理 (Pumping lemma)

对于任意的正则语言 $A$, 如果存在 $p$ (pumping length) 使得 $s in A$ 且 $|s| > p$, 则 $s = x y z$, 其中:
- $x y^i z in A "for all" i >= 0$: 其中 $y^i = underbrace(y y ... y, i)$, $y$ 是相连的重复子串, 且 $y$ 可能不存在.
- $y != epsilon$: $y$ 不是空字符串.
- $|x y| <= p$: $y$ 在位置 $p$ 或 $p$ 之前结束.

换句话说, $s$ 会呈现某种 $x y y dots y z$ 的模式. 如下图所示:

#figure(
  image("img/string_xyz_pattern.png", width: 40%),
  caption: [字符串分割示意图. @mitocw_theory_of_computation],
)

如下图所示, 根据鸽巢原理 (Pigeonhole Principle), DFA 在读取字符串 $s$ 的过程中, 至少会访问某个状态 $q_j$ 两次.

#figure(
  caption: [泵引理示意图.],
  diagram(
    spacing: 3em,

    trans((-1, 0), "r", [Start], label-pos: 0, label-sep: -0.1em),

    state((0, 0), name: <q1>, [$q_1$]),
    state((1, 0), name: <q2>, [$q_j$]),
    accept_state((2, 0), name: <q3>, [$q_3$], extrude: (-2.5, 0)),

    trans(<q1>, <q2>, [$x$], "wave"),
    trans(<q2>, <q2>, [$y$], bend: 130deg, "wave"),
    trans(<q2>, <q3>, [$z$], "wave"),
  )
)

$s$ 必须属于该自动机的语言, 否则可能被提前拒绝 (字符串被截断).

== 上下文无关语法 (Context-free grammars)

下面是上下文无关语法#footnote[此处翻译为大陆地区所使用的 "语法", 而非港台地区所使用的 "文法".] (以下简称 CFG) $G_1$ 的产生式 (production rules):

$
S -> 0 S 1 \
S -> R \
R -> epsilon
$

缩写:

$
S -> 0 S 1 space | space R \
R -> epsilon
$

分析后可知, 其语言为:

$ L(G_1) = { 0^k 1^k | k >= 0 } $

=== 正式定义

一个上下文无关语法是一个四元组 $(V, Sigma, R, S)$, 其中:

- $V$ 是*变量*的有限集.
- $Sigma$ 是*终结字符*的有限集.
- $R: V -> (V union Sigma)^*$ 是*产生式*的有限集.
- $S in V$ 是*初始变量*.

对于 $u, v in (V union Sigma)^*$:

- $u => v$: 可以通过一次替换, 从 $u$ 变成 $v$.
- $u limits(=>)^* v$: 可以通过多次替换, 从 $u$ 变成 $v$, 即 $u => u_1 => u_2 => dots.h.c => u_k => v$.

如果对于某些 CFG $G$ 来说, $A = L(G)$, 则 $A$ 是*上下文无关语言 (context-free language, CFL)*.

$ L(G) = { w | w in Sigma^* "and" S limits(=>)^* w} $

== 下推自动机 (Pushdown automata) <pda>

下推自动机 (以下简称 PDA) 与 NFA 相比, 多了一个栈 (stack).
- 栈顶元素可以作为转移函数的参数: 状态的转移不再仅限于当前状态和输入字符串.
- 可以对栈进行操作: 压栈 (push) 和出栈 (pop).

PDA 的栈是无限大的, 且压栈和出栈都是针对栈顶元素进行的操作, 意味着 PDA 无法直接访问栈中的其他元素.

#figure(
  image("img/pushdown.svg", width: 50%),
  caption: [下推自动机示意图. @wikipedia_pushdown_automaton],
)

PDA 中的输入字符串也被称之为*输入磁带 (input tape)*. \
与有限自动机一样, PDA 只能从头部消耗输入字符串中的字符, 即指向输入磁带的指针初始指向头部, 且只能向右移动.

=== 正式定义

一个 PDA 是一个六元组 $(Q, Sigma, Gamma, delta, q_0, F)$, 其中:
- $Q$ 是状态的有限集.
- $Sigma$ 是由字母表符号组成的有限集.
- $delta: Q times Sigma_epsilon times Gamma_epsilon -> cal(P)(Q times Gamma_epsilon)$ 是转移函数.
- $q_0 in Q$ 是初始状态.
- $F subset.eq Q$ 是接受状态的集合.

其中:
- $Sigma_epsilon$ 表示可以进行 $epsilon$-转移.
- 第一个 $Gamma_epsilon$ 表示弹出堆栈顶部的元素, 如果为 $epsilon$ 则表示不执行出栈操作.
- 第二个 $Gamma_epsilon$ 表示要压入堆栈顶部的元素, 如果为 $epsilon$ 则表示不执行压栈操作.

== CFG 转 PDA

*定理*: 如果 $A$ 是一个 CFL, 有 PDA 能识别 $A$.

PDA 需要根据 CFG 的产生式产生其语言, 然后判断输入是否属于该语言.
- 借助*栈*, PDA 才可以存储替换输入字符串所产生的中间结果, 因为 PDA 无法直接修改输入的字符串.
- 借助*不确定性*, PDA 可以枚举 CFG 的语言.

由于栈只能操作顶部元素, 因此可以只对栈顶的变量进行替换, 直到变成终结字符. \
然后再将顶部的终结字符与输入字符串的头部进行比较, 如果匹配则出栈.

运行步骤大致如下:
+ 将初始变量压入栈内.
+ 如果栈顶元素为:
  - *变量*: 根据产生式进行替换.
  - *终结字符*: 弹出栈顶元素, 读取输入字符.
+ 如果栈为空, 表示接受输入 (出栈的条件为匹配, 所以栈为空表示完全匹配).

#figure(caption: [形式语言与识别/生成器关系表])[
  #table(
    columns: 3,
    table.header[][*识别器*][*生成器*],
    [*正则语言*], [DFA 或 NFA], [正则表达式],
    [*上下文无关语言*], [PDA], [上下文无关语法],
  )
]

== 图灵机 (Turing machine) <tm>

图灵机 (以下简称 TM) 有以下特点:
- 可以通过头部读写.
- 头部可以双向移动.
- 磁带向右无限延展.
- 在输入内容后, 有无限多的 ␣ ⎵ ˽ $bracket.b$.
- 可以在任意时刻接受或拒绝 (无需等到输入全部读取).

=== 正式定义

一个 TM 是一个 7 元组 $(Q, Sigma, Gamma, delta, q_0, q_"acc", q_"rej")$, 其中:
- $Q$ 是状态的有限集.
- $Sigma$ 是由字母表符号组成的有限集.
- $Gamma$ 是由磁带上的字符组成的有限集 ($Sigma subset.eq Gamma$).
- $delta: Q times Gamma -> Q times Gamma times {L, R}$ 是转移函数 ($L$ 表示向左移动, $R$ 表示向右移动).
- $q_0 in Q$ 是初始状态.
- $q_"acc" in Q$ 是接受状态.
- $q_"rej" in Q$ 是拒绝状态.

对于输入 $w$, 可能有三种结果:
- 进入 $q_"acc"$ 中的状态: *接受*.
- 进入 $q_"rej"$ 中的状态: *拒绝*.
- 进入死循环: *拒绝*, 因为这种情况下不可能接受.

*定义*: 如果存在某个图灵机 $M$ 使得 $A = L(M)$, 则 $A$ 是*图灵可识别的*.

*定义*: 如果图灵机 $M$ 在所有输入上都停机，则 $M$ 是一个*判定器*.

*定义*: 如果存在某个图灵判定器 $M$ 使得 $A = L(M)$，则 $A$ 是*图灵可判定的*.
