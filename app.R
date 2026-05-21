# ============================================================
#  PDA Scientific Calculator — R Shiny
#  WARM BRUTALIST / PAPER + INK Aesthetic
#  Unique design: cream paper, ink textures, typewriter fonts
#  Author: [Your Name]
# ============================================================

library(shiny)

# ══════════════════════════════════════════════════════════════
#  PDA STACK ENGINE (Pure R)
# ══════════════════════════════════════════════════════════════

Stack <- function() {
  s <- list()
  list(
    push    = function(x) { s[[length(s)+1]] <<- x },
    pop     = function() {
      if (!length(s)) stop("Stack underflow")
      v <- s[[length(s)]]; s[[length(s)]] <<- NULL; v
    },
    peek    = function() if (!length(s)) NULL else s[[length(s)]],
    isEmpty = function() !length(s),
    toVec   = function() rev(unlist(s)),
    size    = function() length(s)
  )
}

tokenise <- function(expr) {
  expr <- gsub("\\s+","",expr)
  toks <- c(); i <- 1
  while (i <= nchar(expr)) {
    ch <- substr(expr,i,i)
    if (grepl("[0-9\\.]",ch)) {
      j <- i
      while (j<=nchar(expr) && grepl("[0-9\\.]",substr(expr,j,j))) j <- j+1
      toks <- c(toks, substr(expr,i,j-1)); i <- j
    } else if (ch %in% c("(",")")) {
      toks <- c(toks,ch); i <- i+1
    } else if (ch %in% c("+","-","*","/","^","%")) {
      if (ch=="-" && (length(toks)==0 || tail(toks,1) %in% c("(","+"
                                                              ,"-","*","/","^","%")))
        toks <- c(toks,"~")
      else
        toks <- c(toks,ch)
      i <- i+1
    } else {
      m <- regexpr("^(asin|acos|atan|sinh|cosh|tanh|sqrt|log10|log2|log|exp|abs|sin|cos|tan|ln)",
                   substr(expr,i,nchar(expr)))
      if (m>0) {
        fn <- substr(expr,i,i+attr(m,"match.length")-1)
        toks <- c(toks,fn); i <- i+attr(m,"match.length")
      } else stop(paste("Unknown token at pos",i,":",ch))
    }
  }
  toks
}

FUNCTIONS <- c("sin","cos","tan","asin","acos","atan",
                "sinh","cosh","tanh","sqrt","log","log10","log2",
                "ln","exp","abs")

prec_of <- function(op) switch(op,"+"=1,"-"=1,"*"=2,"/"=2,"%"=2,"^"=4,"~"=3,
                               if (op %in% FUNCTIONS) 5 else 0)
is_right<- function(op) op %in% c("^","~")
is_op   <- function(t) t %in% c("+","-","*","/","^","%","~")
is_fn   <- function(t) t %in% FUNCTIONS

infix_to_postfix <- function(tokens) {
  output <- c(); op_stk <- Stack(); pda_log <- list()

  log_step <- function(tok,action) {
    pda_log[[length(pda_log)+1]] <<- list(
      token  = tok,
      stack  = if (op_stk$isEmpty()) "∅" else paste(rev(op_stk$toVec()),collapse=" │ "),
      output = if (!length(output)) "∅" else paste(output,collapse=" "),
      action = action
    )
  }

  for (tok in tokens) {
    if (suppressWarnings(!is.na(as.numeric(tok)))) {
      output <- c(output,tok)
      log_step(tok,"NUMBER  →  enqueue to output")
    } else if (is_fn(tok)) {
      op_stk$push(tok)
      log_step(tok,"FUNCTION  →  push onto stack")
    } else if (tok=="(") {
      op_stk$push(tok)
      log_step(tok,"LEFT PAREN  →  push onto stack")
    } else if (tok==")") {
      while (!op_stk$isEmpty() && op_stk$peek()!="(") output <- c(output,op_stk$pop())
      if (op_stk$isEmpty()) stop("Mismatched parentheses")
      op_stk$pop()
      if (!op_stk$isEmpty() && is_fn(op_stk$peek())) output <- c(output,op_stk$pop())
      log_step(tok,"RIGHT PAREN  →  pop until '('")
    } else if (is_op(tok)) {
      while (!op_stk$isEmpty() && op_stk$peek()!="(" &&
             (is_fn(op_stk$peek()) ||
              prec_of(op_stk$peek())>prec_of(tok) ||
              (prec_of(op_stk$peek())==prec_of(tok) && !is_right(tok))))
        output <- c(output,op_stk$pop())
      op_stk$push(tok)
      log_step(tok,paste0("OPERATOR  →  push (precedence: ",prec_of(tok),")"))
    }
  }
  while (!op_stk$isEmpty()) {
    top <- op_stk$pop()
    if (top=="(") stop("Mismatched parentheses")
    output <- c(output,top)
  }
  log_step("EOF","Drain stack  →  all remaining ops to output")
  list(postfix=output, log=pda_log)
}

eval_postfix <- function(pf) {
  stk <- Stack()
  for (t in pf) {
    n <- suppressWarnings(as.numeric(t))
    if (!is.na(n)) {
      stk$push(n)
    } else if (t=="~") {
      stk$push(-stk$pop())
    } else if (is_op(t)) {
      b <- stk$pop(); a <- stk$pop()
      r <- switch(t,"+"=a+b,"-"=a-b,"*"=a*b,"/"={ if(b==0) stop("Division by zero"); a/b },
                  "^"=a^b,"%"=a%%b)
      stk$push(r)
    } else if (is_fn(t)) {
      a <- stk$pop()
      r <- switch(t,sin=sin(a),cos=cos(a),tan=tan(a),asin=asin(a),acos=acos(a),atan=atan(a),
                  sinh=sinh(a),cosh=cosh(a),tanh=tanh(a),sqrt=sqrt(a),abs=abs(a),exp=exp(a),
                  log=log(a),ln=log(a),log10=log10(a),log2=log2(a),
                  stop(paste("Unknown function:",t)))
      stk$push(r)
    }
  }
  stk$pop()
}

evaluate_expression <- function(expr) {
  tryCatch({
    toks  <- tokenise(expr)
    pf    <- infix_to_postfix(toks)
    res   <- eval_postfix(pf$postfix)
    list(success=TRUE, tokens=toks, postfix=pf$postfix, pda_log=pf$log, result=res)
  }, error=function(e) list(success=FALSE, error=conditionMessage(e)))
}

# ══════════════════════════════════════════════════════════════
#  UI  —  WARM BRUTALIST / PAPER + INK  THEME
# ══════════════════════════════════════════════════════════════

ui <- fluidPage(
  tags$head(
    tags$title("CALCULUS — PDA Scientific Calculator"),
    tags$link(rel="preconnect",href="https://fonts.googleapis.com"),
    tags$link(rel="stylesheet",
      href="https://fonts.googleapis.com/css2?family=Playfair+Display:wght@400;700;900&family=Special+Elite&family=IBM+Plex+Mono:wght@400;600&display=swap"),
    tags$style(HTML('
      :root{
        --ink:#1a0f00;
        --paper:#f5f0e8;
        --cream:#ede8dc;
        --aged:#d4c9b0;
        --rust:#c4451a;
        --forest:#2d5a27;
        --navy:#1a2744;
        --gold:#b8860b;
        --red-ink:#8b1a1a;
        --muted:#7a6e5f;
      }
      *{box-sizing:border-box;margin:0;padding:0}
      html,body{background:var(--paper);color:var(--ink);font-family:"IBM Plex Mono",monospace;min-height:100vh}

      /* paper grain texture overlay */
      body::after{
        content:"";position:fixed;inset:0;pointer-events:none;z-index:9999;
        background-image:url("data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' width='300' height='300'%3E%3Cfilter id='n'%3E%3CfeTurbulence type='fractalNoise' baseFrequency='0.9' numOctaves='4' stitchTiles='stitch'/%3E%3CfeColorMatrix type='saturate' values='0'/%3E%3C/filter%3E%3Crect width='300' height='300' filter='url(%23n)' opacity='0.04'/%3E%3C/svg%3E");
        opacity:.5;mix-blend-mode:multiply;
      }

      .wrapper{max-width:1080px;margin:0 auto;padding:28px 20px;position:relative}

      /* ── HEADER ── */
      .site-header{
        border:3px solid var(--ink);
        padding:20px 28px 16px;
        margin-bottom:28px;
        position:relative;
        background:var(--cream);
      }
      .site-header::before{
        content:"";position:absolute;inset:4px;border:1px solid var(--ink);pointer-events:none;
      }
      .header-eyebrow{font-family:"IBM Plex Mono",monospace;font-size:.65rem;letter-spacing:.3em;
        text-transform:uppercase;color:var(--muted);margin-bottom:4px}
      .header-title{font-family:"Playfair Display",serif;font-size:2.6rem;font-weight:900;
        line-height:1;letter-spacing:-.02em;color:var(--ink)}
      .header-title span{color:var(--rust)}
      .header-sub{font-family:"Special Elite",cursive;font-size:.85rem;color:var(--muted);margin-top:6px}
      .header-rule{height:1px;background:var(--ink);margin:12px 0 4px;position:relative}
      .header-rule::after{content:"PUSHDOWN AUTOMATON EDITION";position:absolute;
        right:0;top:-9px;font-size:.55rem;letter-spacing:.2em;background:var(--cream);
        padding:0 8px;color:var(--muted)}

      /* ── LAYOUT ── */
      .main-grid{display:grid;grid-template-columns:400px 1fr;gap:20px;align-items:start}
      @media(max-width:800px){.main-grid{grid-template-columns:1fr}}

      /* ── CARD ── */
      .ink-card{
        border:2px solid var(--ink);
        background:var(--cream);
        padding:20px;
        position:relative;
      }
      .ink-card::before{
        content:attr(data-label);
        position:absolute;top:-11px;left:14px;
        font-family:"IBM Plex Mono",monospace;font-size:.6rem;letter-spacing:.2em;
        text-transform:uppercase;background:var(--cream);padding:0 8px;
        color:var(--muted);border:1px solid var(--ink);
      }

      /* ── DISPLAY ── */
      .calc-display{
        border:2px solid var(--ink);
        background:var(--paper);
        padding:14px 16px;
        margin-bottom:16px;
        min-height:80px;
        position:relative;
        box-shadow:3px 3px 0 var(--ink);
      }
      .disp-tag{font-size:.55rem;letter-spacing:.25em;text-transform:uppercase;
        color:var(--muted);margin-bottom:5px}
      .disp-expr{font-family:"Special Elite",cursive;font-size:1.7rem;color:var(--ink);
        word-break:break-all;min-height:40px;line-height:1.2}
      .disp-result{font-family:"IBM Plex Mono",monospace;font-size:.95rem;
        color:var(--forest);border-top:1px dashed var(--aged);margin-top:8px;padding-top:7px}
      .disp-result.err{color:var(--rust)}
      .disp-cursor{display:inline-block;width:2px;height:1.2em;background:var(--rust);
        vertical-align:text-bottom;animation:blink .9s step-end infinite;margin-left:1px}
      @keyframes blink{0%,100%{opacity:1}50%{opacity:0}}

      /* ── BUTTONS ── */
      .btn-grid{display:grid;grid-template-columns:repeat(5,1fr);gap:6px;margin-bottom:14px}
      .cb{
        font-family:"IBM Plex Mono",monospace;font-size:.8rem;
        border:1.5px solid var(--ink);background:var(--paper);color:var(--ink);
        padding:10px 4px;cursor:pointer;
        transition:background .08s,transform .08s,box-shadow .08s;
        position:relative;box-shadow:2px 2px 0 var(--ink);
      }
      .cb:hover{background:var(--ink);color:var(--paper);transform:translate(-1px,-1px);box-shadow:3px 3px 0 var(--rust)}
      .cb:active{transform:translate(2px,2px);box-shadow:none}
      .cb.op{border-color:var(--rust);color:var(--rust)}
      .cb.op:hover{background:var(--rust);color:var(--paper)}
      .cb.fn{border-color:var(--navy);color:var(--navy);font-size:.7rem}
      .cb.fn:hover{background:var(--navy);color:var(--paper)}
      .cb.cst{border-color:var(--forest);color:var(--forest)}
      .cb.cst:hover{background:var(--forest);color:var(--paper)}
      .cb.clr{border-color:var(--red-ink);color:var(--red-ink);font-weight:600}
      .cb.clr:hover{background:var(--red-ink);color:var(--paper)}
      .cb.eq{
        background:var(--ink);color:var(--paper);
        font-size:1.1rem;font-weight:600;border-color:var(--ink);
        box-shadow:3px 3px 0 var(--rust);
      }
      .cb.eq:hover{background:var(--rust);border-color:var(--rust);
        color:var(--paper);box-shadow:3px 3px 0 var(--ink)}

      /* ── EXAMPLES ── */
      .ex-row{display:flex;flex-wrap:wrap;gap:5px;margin-top:4px}
      .ex-chip{
        font-family:"IBM Plex Mono",monospace;font-size:.65rem;
        border:1px dashed var(--muted);color:var(--muted);
        padding:3px 9px;cursor:pointer;background:transparent;
        transition:all .1s;
      }
      .ex-chip:hover{border-color:var(--rust);color:var(--rust);border-style:solid}

      /* ── PDA TRACE ── */
      .trace-scroll{max-height:500px;overflow-y:auto;padding-right:6px}
      .trace-scroll::-webkit-scrollbar{width:4px}
      .trace-scroll::-webkit-scrollbar-track{background:var(--paper);border:1px solid var(--aged)}
      .trace-scroll::-webkit-scrollbar-thumb{background:var(--ink)}

      .trace-step{
        display:grid;grid-template-columns:28px 1fr;gap:10px;align-items:start;
        padding:10px 12px;margin-bottom:6px;
        border-left:3px solid var(--ink);
        background:var(--paper);
        animation:slide .2s ease;
      }
      @keyframes slide{from{opacity:0;transform:translateX(-6px)}to{opacity:1;transform:none}}
      .snum{font-family:"Playfair Display",serif;font-size:.85rem;font-weight:700;
        color:var(--rust);padding-top:1px;text-align:right}
      .sbody{display:flex;flex-direction:column;gap:3px;font-size:.72rem}
      .stok{font-family:"Special Elite",cursive;font-size:.9rem;color:var(--ink);font-weight:700}
      .sact{color:var(--forest);letter-spacing:.02em}
      .sstk{color:var(--navy)}
      .sout{color:var(--muted)}
      .sdivider{height:1px;background:var(--aged);margin:2px 0}

      .badges-row{display:flex;gap:8px;flex-wrap:wrap;margin-bottom:12px}
      .stamp{
        font-family:"IBM Plex Mono",monospace;font-size:.68rem;
        border:2px solid;padding:4px 10px;text-transform:uppercase;letter-spacing:.05em;
      }
      .stamp-pf{border-color:var(--navy);color:var(--navy)}
      .stamp-rs{border-color:var(--forest);color:var(--forest);font-weight:600;font-size:.8rem}

      .empty-msg{
        text-align:center;padding:50px 20px;
        font-family:"Special Elite",cursive;font-size:.9rem;
        color:var(--muted);border:1px dashed var(--aged);
        line-height:1.7;
      }
      .empty-msg strong{display:block;font-size:1.3rem;color:var(--aged);margin-bottom:8px}

      /* prec table */
      .prec-table{width:100%;border-collapse:collapse;font-size:.7rem;margin-top:8px}
      .prec-table th{font-family:"Playfair Display",serif;border-bottom:1.5px solid var(--ink);
        padding:4px 8px;text-align:left;font-size:.7rem;letter-spacing:.05em}
      .prec-table td{padding:3px 8px;border-bottom:1px dashed var(--aged);
        font-family:"IBM Plex Mono",monospace}
      .prec-table tr:hover td{background:var(--paper)}
    '))
  ),

  div(class="wrapper",
    # ── HEADER ─────────────────────────────────────────────
    div(class="site-header",
      div(class="header-eyebrow","Computer Science  ·  Theory of Computation  ·  R Language"),
      div(class="header-title",
        "CALC", tags$span("ULUS")
      ),
      div(class="header-sub",
        "A Scientific Calculator using Pushdown Automaton Stack for Infix → Postfix Conversion"
      ),
      div(class="header-rule")
    ),

    div(class="main-grid",

      # ── LEFT: Calculator ─────────────────────────────────
      div(class="ink-card", `data-label`="INPUT DEVICE",

        # Display
        div(class="calc-display",
          div(class="disp-tag","expression"),
          div(class="disp-expr",
            textOutput("display_expr", inline=TRUE),
            tags$span(class="disp-cursor")
          ),
          div(class="disp-result", uiOutput("display_result"))
        ),

        div(class="btn-grid",
          actionButton("b_sin","sin",  class="cb fn"),
          actionButton("b_cos","cos",  class="cb fn"),
          actionButton("b_tan","tan",  class="cb fn"),
          actionButton("b_sqrt","√",   class="cb fn"),
          actionButton("b_log","log",  class="cb fn"),
          actionButton("b_ln","ln",    class="cb fn"),
          actionButton("b_exp","exp",  class="cb fn"),
          actionButton("b_abs","abs",  class="cb fn"),
          actionButton("b_pi","π",     class="cb cst"),
          actionButton("b_e","ℯ",      class="cb cst"),
          actionButton("b_lp","(",     class="cb op"),
          actionButton("b_rp",")",     class="cb op"),
          actionButton("b_pct","%",    class="cb op"),
          actionButton("b_pow","xʸ",   class="cb op"),
          actionButton("b_clr","CLR",  class="cb clr"),
          actionButton("b_7","7",      class="cb"),
          actionButton("b_8","8",      class="cb"),
          actionButton("b_9","9",      class="cb"),
          actionButton("b_div","÷",    class="cb op"),
          actionButton("b_bs","⌫",     class="cb clr"),
          actionButton("b_4","4",      class="cb"),
          actionButton("b_5","5",      class="cb"),
          actionButton("b_6","6",      class="cb"),
          actionButton("b_mul","×",    class="cb op"),
          actionButton("b_asin","asin",class="cb fn"),
          actionButton("b_1","1",      class="cb"),
          actionButton("b_2","2",      class="cb"),
          actionButton("b_3","3",      class="cb"),
          actionButton("b_sub","−",    class="cb op"),
          actionButton("b_atan","atan",class="cb fn"),
          actionButton("b_0","0",      class="cb"),
          actionButton("b_dot",".",    class="cb"),
          actionButton("b_neg","+/−",  class="cb op"),
          actionButton("b_add","+",    class="cb op"),
          actionButton("b_eq","=",     class="cb eq")
        ),

        tags$hr(style="border:none;border-top:1px dashed var(--aged);margin:10px 0"),

        div(style="font-size:.6rem;letter-spacing:.2em;text-transform:uppercase;color:var(--muted);margin-bottom:6px",
          "Quick Expressions"),
        div(class="ex-row",
          tags$span(class="ex-chip","3+4*2/(1-5)^2",
            onclick="Shiny.setInputValue('chip_in',this.textContent,{priority:'event'})"),
          tags$span(class="ex-chip","sin(3.14/2)+cos(0)",
            onclick="Shiny.setInputValue('chip_in',this.textContent,{priority:'event'})"),
          tags$span(class="ex-chip","sqrt(16)+log(100)",
            onclick="Shiny.setInputValue('chip_in',this.textContent,{priority:'event'})"),
          tags$span(class="ex-chip","2^10-1",
            onclick="Shiny.setInputValue('chip_in',this.textContent,{priority:'event'})"),
          tags$span(class="ex-chip","exp(1)*ln(exp(1))",
            onclick="Shiny.setInputValue('chip_in',this.textContent,{priority:'event'})")
        )
      ),

      # ── RIGHT: Trace + Info ──────────────────────────────
      tagList(
        div(class="ink-card",`data-label`="PDA STACK TRACE",
          style="margin-bottom:18px",
          uiOutput("trace_badges"),
          div(class="trace-scroll", uiOutput("pda_trace"))
        ),

        div(class="ink-card",`data-label`="PRECEDENCE TABLE",
          tags$table(class="prec-table",
            tags$thead(tags$tr(
              tags$th("Operator"), tags$th("Symbols"), tags$th("Prec."), tags$th("Assoc.")
            )),
            tags$tbody(
              tags$tr(tags$td("Addition/Sub"),tags$td("+ −"),tags$td("1"),tags$td("Left")),
              tags$tr(tags$td("Multiply/Div"),tags$td("× ÷ %"),tags$td("2"),tags$td("Left")),
              tags$tr(tags$td("Unary Minus"), tags$td("~"),  tags$td("3"),tags$td("Right")),
              tags$tr(tags$td("Exponent"),    tags$td("^"),  tags$td("4"),tags$td("Right")),
              tags$tr(tags$td("Functions"),   tags$td("sin, cos …"),tags$td("5"),tags$td("—"))
            )
          )
        )
      )
    )
  )
)

# ══════════════════════════════════════════════════════════════
#  SERVER
# ══════════════════════════════════════════════════════════════

server <- function(input, output, session) {
  state <- reactiveValues(expr="", result=NULL, pda=NULL, postfix=NULL, error=NULL)

  reset_trace <- function(){
    state$result <- NULL; state$pda <- NULL; state$postfix <- NULL; state$error <- NULL
  }

  btn_map <- list(
    b_0="0",b_1="1",b_2="2",b_3="3",b_4="4",b_5="5",b_6="6",b_7="7",b_8="8",b_9="9",
    b_dot=".",b_add="+",b_sub="-",b_mul="*",b_div="/",b_pow="^",b_pct="%",
    b_lp="(",b_rp=")",
    b_sin="sin(",b_cos="cos(",b_tan="tan(",b_asin="asin(",b_atan="atan(",
    b_sqrt="sqrt(",b_log="log(",b_ln="ln(",b_exp="exp(",b_abs="abs(",
    b_pi="3.14159265",b_e="2.71828183"
  )

  lapply(names(btn_map), function(id)
    observeEvent(input[[id]],{state$expr<-paste0(state$expr,btn_map[[id]]);reset_trace()},ignoreInit=TRUE))

  observeEvent(input$b_clr,  {state$expr<-"";reset_trace()})
  observeEvent(input$b_bs,   {if(nchar(state$expr)>0)state$expr<-substr(state$expr,1,nchar(state$expr)-1);reset_trace()})
  observeEvent(input$b_neg,  {if(nchar(state$expr)>0)state$expr<-paste0("-("+state$expr+")")})
  observeEvent(input$chip_in,{state$expr<-input$chip_in;reset_trace()})

  observeEvent(input$b_eq,{
    req(nchar(state$expr)>0)
    res <- evaluate_expression(state$expr)
    if(res$success){
      state$result  <- res$result
      state$pda     <- res$pda_log
      state$postfix <- res$postfix
      state$error   <- NULL
    } else {
      state$error <- res$error
      state$result <- state$pda <- state$postfix <- NULL
    }
  })

  output$display_expr <- renderText({ if(!nchar(state$expr)) "0" else state$expr })

  output$display_result <- renderUI({
    if(!is.null(state$error))
      div(class="disp-result err", paste("Error:", state$error))
    else if(!is.null(state$result))
      div(class="disp-result", paste("=", format(state$result,digits=10,scientific=FALSE)))
    else
      div(class="disp-result", style="color:var(--aged)", "awaiting evaluation…")
  })

  output$trace_badges <- renderUI({
    if(is.null(state$pda)) return(
      div(class="empty-msg",
        tags$strong("— No trace yet —"),
        "Enter an expression and press  =",br(),"to watch the PDA process it step by step."
      )
    )
    tagList(
      div(class="badges-row",
        span(class="stamp stamp-pf", paste("Postfix:", paste(isolate(state$postfix),collapse=" "))),
        span(class="stamp stamp-rs", paste("Result =", format(isolate(state$result),digits=10)))
      )
    )
  })

  output$pda_trace <- renderUI({
    if(is.null(state$pda)) return(NULL)
    tagList(lapply(seq_along(state$pda), function(i){
      s <- state$pda[[i]]
      div(class="trace-step",
        div(class="snum", i),
        div(class="sbody",
          div(class="stok",  paste("token →", s$token)),
          div(class="sact",  paste("▸", s$action)),
          div(class="sdivider"),
          div(class="sstk",  paste("stack:", s$stack)),
          div(class="sout",  paste("queue:", s$output))
        )
      )
    }))
  })
}

shinyApp(ui=ui, server=server)
