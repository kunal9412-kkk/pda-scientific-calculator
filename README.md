# 🧮 CALCULUS — PDA Scientific Calculator

<div align="center">

![R](https://img.shields.io/badge/R-Shiny-276DC3?style=for-the-badge&logo=r&logoColor=white)
![License](https://img.shields.io/badge/License-MIT-green?style=for-the-badge)
![Theory](https://img.shields.io/badge/Theory-Pushdown%20Automaton-8B1A1A?style=for-the-badge)
![Status](https://img.shields.io/badge/Status-Production%20Ready-2D5A27?style=for-the-badge)

**A scientific calculator that evaluates complex infix expressions by converting them to postfix using a Pushdown Automaton (PDA) stack — built entirely in R/Shiny.**

[Features](#features) · [How It Works](#how-it-works) · [Installation](#installation) · [Usage](#usage) · [Screenshots](#screenshots)

</div>

---

## ✨ Features

| Feature | Details |
|---------|---------|
| 🧠 **PDA Stack Engine** | Full Shunting-Yard algorithm implemented in pure R |
| 📋 **Step-by-step Trace** | Every PDA state change logged and displayed live |
| 🔢 **Scientific Functions** | sin, cos, tan, asin, acos, atan, sinh, cosh, tanh, sqrt, log, ln, log2, log10, exp, abs |
| ✏️ **Paper & Ink UI** | Distinctive warm brutalist design — typewriter fonts, stamp aesthetics |
| ⌨️ **Keyboard Support** | Full keyboard input for power users |
| 📐 **Precedence Table** | Built-in operator precedence reference |
| 🎯 **Example Expressions** | One-click example expressions to explore |

---

## 🧠 How It Works

### Theory: Pushdown Automaton (PDA)

A **Pushdown Automaton** is a finite state machine augmented with a stack memory. For expression parsing, the PDA uses two data structures:

```
Input tokens ──→ [ PDA ] ──→ Output Queue (postfix)
                    ↕
                [ STACK ]
```

### Algorithm: Shunting-Yard

The app implements Dijkstra's **Shunting-Yard Algorithm** as the PDA transition function:

```
For each token in infix expression:
  
  NUMBER   → push directly to output queue
  FUNCTION → push onto operator stack
  (        → push onto operator stack  
  )        → pop stack → output until matching (
  OPERATOR → while stack top has higher/equal precedence:
                 pop stack → output
             push current operator onto stack
  EOF      → drain remaining stack → output
```

### Operator Precedence

| Operator | Symbols | Precedence | Associativity |
|----------|---------|-----------|--------------|
| Additive | `+` `−` | 1 | Left |
| Multiplicative | `×` `÷` `%` | 2 | Left |
| Unary minus | `~` | 3 | Right |
| Exponent | `^` | 4 | Right |
| Functions | `sin`, `cos`, … | 5 | — |

### Example Walkthrough

Input: `3 + 4 * 2`

| Step | Token | Action | Stack | Output Queue |
|------|-------|--------|-------|-------------|
| 1 | `3` | NUMBER → output | `∅` | `3` |
| 2 | `+` | OPERATOR push (prec 1) | `+` | `3` |
| 3 | `4` | NUMBER → output | `+` | `3 4` |
| 4 | `*` | OPERATOR push (prec 2 > 1) | `+ *` | `3 4` |
| 5 | `2` | NUMBER → output | `+ *` | `3 4 2` |
| 6 | EOF | Drain stack | `∅` | `3 4 2 * +` |

**Postfix:** `3 4 2 * +` → evaluates to `3 + (4×2) = 11` ✓

---

## 🚀 Installation

### Prerequisites

- R ≥ 4.0.0
- Shiny package

### Install & Run

```r
# 1. Install dependencies
install.packages("shiny")

# 2. Clone this repository
# git clone https://github.com/YOUR_USERNAME/pda-scientific-calculator.git

# 3. Run the app
library(shiny)
runApp("app.R")
```

### Or run directly from GitHub

```r
library(shiny)
runGitHub("pda-scientific-calculator", "YOUR_USERNAME")
```

---

## 📁 Project Structure

```
pda-scientific-calculator/
│
├── app.R           # Complete application (UI + Server + PDA Engine)
├── README.md       # This file
└── .gitignore      # R/Shiny gitignore
```

The entire PDA logic is self-contained in `app.R`, organized as:

```
app.R
├── Stack()                    # Closure-based stack factory
├── tokenise(expr)             # Lexer — splits expression into tokens
├── infix_to_postfix(tokens)   # Shunting-Yard PDA with step logging
├── eval_postfix(postfix)      # Stack-based postfix evaluator
├── evaluate_expression(expr)  # Top-level orchestrator
├── ui                         # Shiny UI definition
└── server                     # Shiny server logic
```

---

## 🧪 Supported Expressions

```r
# Arithmetic
"3 + 4 * 2 / (1 - 5)^2"     # → 3.5

# Trigonometry (radians)
"sin(3.14159265/2) + cos(0)" # → 2

# Logarithms
"sqrt(16) + log(100)"        # → 8.605...

# Powers
"2^10 - 1"                   # → 1023

# Nested functions
"exp(1) * ln(exp(1))"        # → 2.71828...

# Unary minus
"-(3 + 4) * 2"               # → -14

# Modulo
"17 % 5"                     # → 2
```

---

## 🎨 Design Philosophy

The UI adopts a **Warm Brutalist / Paper & Ink** aesthetic:

- **Typography**: Playfair Display (display) · Special Elite (typewriter) · IBM Plex Mono (code)
- **Color Palette**: Aged cream paper, india ink, rust red, forest green, navy
- **Texture**: SVG noise grain overlay simulating physical paper
- **Interactions**: Stamp-press button effects with offset box-shadows
- **Trace Panel**: Every PDA step rendered as a newspaper-clipping card

This design makes the academic/CS nature of the project visually distinct and memorable.

---

## 📖 Academic Context

This project demonstrates:

1. **Formal Language Theory** — PDA as a computational model
2. **Parsing Theory** — Operator precedence parsing
3. **Stack Data Structure** — Practical application of LIFO stacks
4. **Functional R Programming** — Closures as OOP-like objects

### References

- Dijkstra, E. W. (1961). *Algol 60 translation*. Mathematisch Centrum
- Hopcroft, J., Ullman, J. (1979). *Introduction to Automata Theory, Languages and Computation*
- [Shiny Documentation](https://shiny.posit.co/)

---

## 📄 License

MIT License — see [LICENSE](LICENSE) for details.

---

<div align="center">
Made with ❤️ in R &nbsp;·&nbsp; Theory of Computation Project
</div>
