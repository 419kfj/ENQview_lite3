#
#　ENQview_lite ver2.0
#
# 履歴
#　ver2.0-　2025/10/08
#' 履歴
#' 2025/10/06 昨日2025/10/05の最終版
#' 2025/06/17 基本集計のカテゴリから「非該当」を除くスクリプトを暫定的にいれた。ちょっと乱暴。
#' 2025/05/27 tableに投入するdfの参照を[,vari]から[[vari]]に変更。これでエラーがでなくなった。
#' 2024/12/22 function化、パッケージ化、一段落
#' 2024/12/21 function化作業開始
#' 2024/01/30 gitでの管理　を開始

options(shiny.sanitize.errors = FALSE)
library(shiny)
library(tidyverse)
library(gt)
library(gtsummary)
library(vcd)
library(purrr)
library(tibble)
library(showtext)
showtext::showtext_auto(TRUE)

if (file.exists("DESCRIPTION")) {
  desc_info <- read.dcf("DESCRIPTION")
  app_version <- desc_info[1, "Version"]
} else {
  app_version <- "3.xx-explor-fix"
}

ui <- fluidPage(
  titlePanel(paste("ENQview_lite  Version:", app_version)),

  tabPanel(
    "基本集計",
    sidebarLayout(   # ← ★ ここを sidebarLayout() に変更！
      sidebarPanel(
        fileInput("rda_file", "RDAファイルを選択", accept = ".rda"),
        uiOutput("rda_objects_ui"),
        uiOutput("variables_ui"),
        tags$hr(),
        uiOutput("cross_var_ui"),
        uiOutput("layer_var_ui"),
        uiOutput("variables2_ui"),
        uiOutput("hist_var_ui")
      ),

      mainPanel(
        tabsetPanel(
          type = "tabs",

          tabPanel("単変数集計",
                   h2("棒グラフと度数分布"),
                   plotOutput("barchart"),
                   DT::dataTableOutput("simple_table")
          ),

          tabPanel("2変数分析",
                   h2("クロス集計（gtsummary::tbl_cross )"),
                   gt_output(outputId = "my_gt_table2"),
                   plotOutput("crosschart", width = 600, height = 600),
                   h3("χ2乗検定"),
                   verbatimTextOutput("chisq_test2"),
                   h3("クロス表の対応分析"),
                   plotOutput("Crosstable_CA")
          ),

          tabPanel("pairs",
                   h2("GGally::pairs"),
                   plotOutput("pairs", width = 600, height = 600)
          ),

          tabPanel("pairs_multi",
                   h2("GGally::pairs 多変数"),
                   plotOutput("pairs_multi", width = 900, height = 900)
          ),

          tabPanel("2変数分析（層化）",
                   h2("クロス集計（gtsummary::tbl_cross )"),
                   gt_output(outputId = "my_gt_table"),
                   plotOutput("crosschart2", width = 900, height = 600),
                   h3("χ2乗検定"),
                   verbatimTextOutput("chisq_test")
          ),

          tabPanel("MA plot(Bar)",
                   h2("MA変数集計"),
                   plotOutput("MAplot", width = 600, height = 600)
          ),

          tabPanel("MA plot(Dot)",
                   h2("MA変数集計"),
                   plotOutput("MAplot_Dot", width = 600, height = 600)
          ),

          tabPanel("層化 MA plot",
                   h2("層化MA変数集計"),
                   plotOutput("MAplot_lineDot", width = 600, height = 400),
                   plotOutput("MAplot_lineDotwarp", width = 600, height = 600)
          ),

          tabPanel("層化 MA plot2",
                   h2("層化MA変数集計（Legendなし）"),
                   plotOutput("MAplot_lineDot2", width = 600, height = 400),
                   plotOutput("MAplot_lineDotwarp", width = 600, height = 600)
          ),

          tabPanel("Grid回答 General mosaic表示",
                   h2("Grid回答mosaic表示"),
                   plotOutput("GridAnswerG_mosaic", width = 600, height = 600),
                   plotOutput("GridAnswerG_CA", width = 700, height = 700)
          ),

          tabPanel("単変数check",
                   h2("棒グラフと度数分布"),
                   plotOutput("barchart2"),
                   DT::dataTableOutput("simple_table2")
          ),

          tabPanel("選択変数のデータ一覧",
                   h2("データ一覧"),
                   DT::dataTableOutput("table_for_plot")
          )
        )  # ← tabsetPanel の閉じかっこ
      )    # ← mainPanel の閉じかっこ
    )      # ← sidebarLayout の閉じかっこ
  )        # ← tabPanel("基本集計") の閉じかっこ
)          # ← fluidPage の閉じかっこ


server <- function(input, output, session) {
# .rdaファイルから処理対象のdatasetを選択する------
  # アップロードされた RDA を一時的な環境にロード
  rda_env <- reactive({
    req(input$rda_file)
    env <- new.env()
    load(input$rda_file$datapath, envir = env)
    env
  })

  # RDA 内のオブジェクト一覧を選択させる UI
  output$rda_objects_ui <- renderUI({
    req(rda_env())
    selectInput("rda_object", "RDA 内のオブジェクトを選択",
                choices = ls(rda_env()), selected = ls(rda_env())[1])
  })

  # 選択されたオブジェクトを reactiveVal に格納
  data.df <- reactiveVal(NULL)

  observeEvent(input$rda_object, {
    req(input$rda_object)
    obj <- get(input$rda_object, envir = rda_env())
    if (is.data.frame(obj)) {
      data.df(obj)  # ここで reactiveVal に保存
    } else {
      showNotification("選択したオブジェクトはデータフレームではありません。", type = "error")
      data.df(NULL)
    }
  })

# ←★ ここを追加：data.df() が更新されたらUIを更新
  observeEvent(data.df(), {
    req(data.df())
    data <- data.df()

    updateSelectInput(session, "select_input_data_for_hist",
                      choices = colnames(data))
    updateSelectInput(session, "select_input_data_for_cross",
                      choices = c(" ", colnames(data)))
    updateSelectInput(session, "select_input_data_for_layer",
                      choices = c(" ", colnames(data)))
    updateSelectInput(session, "variables",
                      choices = colnames(data),
                      selected = colnames(data)[3:4])
  })

  data_for_plot <- reactive({
    req(data.df())
    data.df()

  })

  output$hist_var_ui <- renderUI({
    req(data.df())
    selectInput("select_input_data_for_hist", "確認したい単変数",
                choices = names(data.df()))
  })

  output$cross_var_ui <- renderUI({
    req(data.df())
    selectInput("select_input_data_for_cross", "クロス集計変数を選択",
                choices = c(" ", names(data.df())))
  })

  output$layer_var_ui <- renderUI({
    req(data.df())
    selectInput("select_input_data_for_layer", "層変数を選択",
                choices = c(" ", names(data.df())))
  })

  # output$variable_ui <- renderUI({
  #   req(data.df())
  #   selectInput("select_input_data_for_layer", "層変数を選択",
  #               choices = c(" ", names(data.df())))
  # })


  # データフレームの列をチェックボックスで選択
  output$variables_ui <- renderUI({
    req(data.df())
#    checkboxGroupInput("variables", "列を選択",
#    radioButtons("variables", "列を選択",
    selectInput("variables", "変数を選択",
                       choices = names(data.df()),
                       selected = names(data.df())[1],
                       multiple = TRUE,
                       selectize = FALSE,
                       size = 7)
  })

  ####---- functions for each output

  # 選択列のプレビュー
  output$table_preview <- renderTable({
    req(data.df(), input$variables)
    data.df()[, input$variables, drop = FALSE]
  })

  # barplot by ggplot2 動作確認用function
  output$barchart <- renderPlot({
    data_for_plot() %>% count(!!!rlang::syms(input$variables[1])) %>%
      dplyr::rename(V1=1) %>%  filter(V1 != "非該当") %>%
      mutate(rate=100 * .data[["n"]]/sum(.data[["n"]])) %>%
      ggplot2::ggplot(aes(x=V1,y=rate)) + ggplot2::geom_col(aes(fill=V1)) +
      ggplot2::ggtitle(input$variables[1])
  })

  output$barchart2 <- renderPlot({
    data_for_plot() %>% count(!!!rlang::syms(input$select_input_data_for_hist)) %>% dplyr::rename(V1=1) %>% filter(V1 != "非該当") %>%
      dplyr::mutate(rate=100* .data[["n"]]/ sum(.data[["n"]])) %>%
      ggplot2::ggplot(aes(x=V1,y=rate)) + ggplot2::geom_col(aes(fill=V1)) + ggplot2::ggtitle(input$select_input_data_for_hist)
  })



  # GGally::ggpairs
  output$pairs <- renderPlot({
    data_for_plot()[,c(input$variables[1],input$select_input_data_for_cross)] %>%
      GGally::ggpairs(mapping = aes(color = !!as.name(input$variables[1]))) +
      ggplot2::theme(axis.text.x = element_text(angle=45,hjust = 1)) +
      ggplot2::ggtitle(input$variables[1]) -> p
    p
  })

  output$pairs_multi<- renderPlot({
    data_for_plot()[,input$variables] %>%
      GGally::ggpairs(mapping = aes(color = !!as.name(input$variables))) +
      ggplot2::theme(axis.text.x = element_text(angle=45,hjust = 1)) +
      ggplot2::ggtitle(input$variables) -> p
    p
  })

  # CA 2026/05/19追記/UIにも
  output$Crosstable_CA <- shiny::renderPlot({
    res.CA <- FactoMineR::CA(table(
      data_for_plot()[[input$variables[1]]],
      data_for_plot()[[input$select_input_data_for_cross]]
    )
    )
  })



  # mosaic plot
  output$crosschart <- renderPlot({
    .tbl <- table(data_for_plot()[[input$select_input_data_for_cross]],
                  data_for_plot()[[input$variables[1]]])  #select_input_data_for_hist
    .tbl.p <- round(100 * prop.table(.tbl ,margin = 1),1)
    tab <- ifelse(.tbl.p < 1, NA, .tbl.p)

    data_for_plot()[,c(input$select_input_data_for_cross,input$variables[1])] %>% #,     #select_input_data_for_hist,
      vcd::structable() %>%
      vcd::mosaic(shade=TRUE,las=2,
                  labeling=labeling_values
      )
  })
  # 層化mosaic plot
  output$crosschart2 <- renderPlot({
    .tbl <- table(data_for_plot()[[input$select_input_data_for_cross]],
                  data_for_plot()[[input$variables[1]]])  # select_input_data_for_hist
    .tbl.p <- round(100 * prop.table(.tbl ,margin = 1),1)
    tab <- ifelse(.tbl.p < 1, NA, .tbl.p)

    data_for_plot()[,c(input$variables[1], #select_input_data_for_hist,
                       input$select_input_data_for_cross,
                       input$select_input_data_for_layer)] %>%
      vcd::structable() %>%
      vcd::mosaic(condvars = 3, # input$select_input_data_for_layer を指定
                  split_vertical = TRUE,# 分割は垂直
                  shade=TRUE,las=2,
                  labeling=labeling_values
      )
  })

  # gtsummary でクロス表表示
  output$my_gt_table <- render_gt(
    data_for_plot() %>% tbl_cross(col = input$select_input_data_for_cross,
                                  row = input$variables[1], #select_input_data_for_hist,
                                  percent = "row") %>%
      add_p(test="chisq.test") %>%
      bold_labels() %>%
      as_gt()
  )

  output$my_gt_table2 <- render_gt(
    data_for_plot() %>% tbl_cross(row = input$select_input_data_for_cross,
                                  col = input$variables[1], #select_input_data_for_hist,
                                  percent = "row") %>%
      add_p(test="chisq.test") %>%
      bold_labels() %>%
      as_gt()
  )


  # chisq.test
  output$chisq_test <- renderPrint({
    res.chisq <- chisq.test(table(data_for_plot()[[input$select_input_data_for_cross]],
                                  data_for_plot()[[input$variables[1]]]),correct = FALSE)  #select_input_data_for_hist]))
    print(res.chisq)
  })

  output$chisq_test2 <- renderPrint({
    res.chisq <- chisq.test(table(data_for_plot()[[input$select_input_data_for_cross]],
                                  data_for_plot()[[input$variables[1]]]),correct = FALSE)  #select_input_data_for_hist]))
    print(res.chisq)
  })



  # gtsummay でMA表
  output$MA_gt_table <- render_gt(
    data_for_plot() %>% tbl_cross(row = input$select_input_data_for_cross,
                                  col = input$variables[1],#select_input_data_for_hist,
                                  percent = "row") %>%
      add_p(test="chisq.test") %>%
      bold_labels() %>%
      as_gt()
  )

  #    MA plot
  output$MAplot <- renderPlot({
    selected_vars <- input$variables  # 選択された変数を取得
    if (length(selected_vars) > 0) {
      # 選択された変数を用いてプロット
      selected_data <- data_for_plot()[, selected_vars, drop = FALSE]
      selected_data %>% dplyr::summarise(across(everything(), ~ mean(. == 1,na.rm =TRUE))) %>%
        tidyr::pivot_longer(cols = everything(), names_to = "Question", values_to = "Ratio") -> ratio_df

      ggplot2::ggplot(ratio_df, aes(x = Question, y = Ratio)) +
        ggplot2::geom_bar(stat = "identity", fill = "skyblue") +
        ggplot2::scale_y_continuous(labels = scales::percent_format()) +
        ggplot2::labs(title = selected_vars,
                      x = "質問項目",
                      y = "割合（% )") +
        ggplot2::theme_minimal()
    }
  })

  # MAplot Cleverland Dot Plot

  output$MAplot_Dot <- renderPlot({
    selected_vars <- input$variables  # 選択された変数を取得
    if (length(selected_vars) > 0) {
      # 選択された変数を用いてプロット
      selected_data <- data_for_plot()[, selected_vars, drop = FALSE]
      selected_data %>% dplyr::summarise(across(everything(), ~ mean(. == 1,na.rm =TRUE))) %>%
        tidyr::pivot_longer(cols = everything(), names_to = "Question", values_to = "Ratio") -> ratio_df

      ratio_df %>%
        ggplot2::ggplot(aes(x=Ratio, y=reorder(Question,Ratio))) + # 並べ替え
        ggplot2::geom_point(size=3) +
        ggplot2::theme_bw() +
        ggplot2::theme(panel.grid.major.x = element_blank(),
                       panel.grid.minor.x = element_blank(),
                       panel.grid.major.y = element_line(colour="grey60",linetype="dashed")) +
        ggplot2::labs(title = selected_vars,
                      x = "割合（% )",y = "質問項目")
    }
  })

  # 層化MA折れ線グラフ
  output$MAplot_lineDot <- renderPlot({
    selected_vars <- input$variables
    if (length(selected_vars) > 0) {
      selected_data <- data_for_plot()[, selected_vars, drop = FALSE]
      gp_vari <- input$select_input_data_for_layer # 層化変数
      data_for_plot() %>% group_by(!!!rlang::syms(gp_vari)) %>%
        dplyr::summarise(度数=n(),across(selected_vars, ~ sum(. == 1,na.rm = TRUE)/n(),.names="ratio_{col}")) -> MA_group_tbl # ここで、行を選択すればいよい
      MA_group_tbl %>% select(-度数) %>%
        tidyr::pivot_longer(cols = starts_with("ratio_"),  # ratio_で始まる列 (変数1〜8) をlong形式に変換
                            names_to = "variable",         # 変数名の列を"variable"として格納
                            values_to = "value") -> df_long

      ggplot2::ggplot(df_long, aes(x = !!as.name(gp_vari), y = value, #color = variable,
                                   shape =variable, group = variable)) +
        ggplot2::geom_line(aes(color = variable)) +  # 折れ線グラフ
        ggplot2::geom_point(aes(color = variable),size=4) + # ポイントを追加（必要なら )
        ggplot2::labs(x = gp_vari, y = "割合", shape = "変数",color = "変数") +  # 軸ラベルと凡例の設定
        ggplot2::theme_minimal() +  # 見た目をシンプルに
        ggplot2::scale_color_discrete() +
        #  scale_color_discrete(labels = names(df)[74:89]) + # 変数のラベルを設定
        ggplot2::scale_shape_manual(values = 1:length(selected_vars))
    }
  })

  # 層化MA折れ線グラフ(legend なし)
  output$MAplot_lineDot2 <- renderPlot({
    selected_vars <- input$variables
    if (length(selected_vars) > 0) {
      selected_data <- data_for_plot()[, selected_vars, drop = FALSE]
      gp_vari <- input$select_input_data_for_layer # 層化変数
      data_for_plot() %>% group_by(!!!rlang::syms(gp_vari)) %>%
        dplyr::summarise(度数=n(),across(selected_vars, ~ sum(. == 1,na.rm = TRUE)/n(),.names="ratio_{col}")) -> MA_group_tbl # ここで、行を選択すればいよい
      MA_group_tbl %>% select(-度数) %>%
        tidyr::pivot_longer(cols = starts_with("ratio_"),  # ratio_で始まる列 (変数1〜8) をlong形式に変換
                            names_to = "variable",         # 変数名の列を"variable"として格納
                            values_to = "value") -> df_long

      ggplot2::ggplot(df_long, aes(x = !!as.name(gp_vari), y = value, #color = variable,
                                   shape =variable, group = variable)) +
        ggplot2::geom_line(aes(color = variable)) +  # 折れ線グラフ
        ggplot2::geom_point(aes(color = variable),size=4) + # ポイントを追加（必要なら )
        ggplot2::labs(x = gp_vari, y = "割合", shape = "変数",color = "変数") +  # 軸ラベルと凡例の設定
        ggplot2::theme_minimal() +  # 見た目をシンプルに
        ggplot2::scale_color_discrete() +
        #  scale_color_discrete(labels = names(df)[74:89]) + # 変数のラベルを設定
        ggplot2::scale_shape_manual(values = 1:length(selected_vars)) +
        ggplot2::theme(legend.position = 'none')
    }
  })


  # 層化MA warp Faset

  output$MAplot_lineDotwarp <- renderPlot({
    selected_vars <- input$variables # 選択された変数群
    if (length(selected_vars) > 0) {
      selected_data <- data_for_plot()[, selected_vars, drop = FALSE]

      gp_vari <- input$select_input_data_for_layer # 層化する変数
      data_for_plot() %>% dplyr::group_by(!!!rlang::syms(gp_vari)) %>%
        dplyr::summarise(度数=n(),across(selected_vars, ~ sum(. == 1,na.rm = TRUE)/n(),.names="ratio_{col}")) -> MA_group_tbl

      MA_group_tbl %>% select(-度数) %>%
        tidyr::pivot_longer(cols = starts_with("ratio_"),  # ratio_で始まる列 (変数1〜8) をlong形式に変換
                            names_to = "variable",         # 変数名の列を"variable"として格納
                            values_to = "value") -> df_long


      ggplot2::ggplot(df_long, aes(x = !!as.name(gp_vari), y = value, color = variable, group = variable)) +
        ggplot2::geom_line() + geom_point() +
        ggplot2::facet_wrap(~ variable,ncol=3) +# scales = "free_y") + # 各変数ごとにfacetで分割
        ggplot2::labs(x = "Group", y = "Value") +
        ggplot2::theme_minimal() +
        ggplot2::theme(legend.position = "none",axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))  # ラベルを90度回転

    }
  })


  # Grid Mosaic
  #

  output$GridAnswer_mosaic <- renderPlot({
    req(input$variables)
    selected_vars <- input$variables
    count_categories <- function(x) {
      table(factor(x, levels = c("++", "+", "-", "--", "DK", "無回答"), exclude = NULL))
    }

    # df の 1 列目から 20 列目の各列ごとにカテゴリを集計
    category_count_tbl <- data_for_plot() %>%
      #dplyr::summarise(across(selected_vars, ~ count_categories(.)))
    　dplyr::reframe(across(selected_vars, ~ count_categories(.)))

    category_count_tbl %>% as.matrix() -> cat_tbl
    rownames(cat_tbl) <- c("++","+","-","--","DK","無回答")
    cat_tbl

    rownames(t(cat_tbl)) -> rnames
    t(cat_tbl) %>% tibble::as.tibble() %>% mutate(ID=rnames,IDn=1:length(rnames)) %>%
      dplyr::mutate(Like=`++`+`+`) %>%
      dplyr::arrange(desc(Like)) %>%
      dplyr::select(IDn) %>% unlist %>%
      setNames(NULL) -> order_vec

    t(cat_tbl)[order_vec,] %>%
      vcd::mosaic(shade = TRUE,rot_labels = c(0, 0),
                  margins=c(left=9,top=5),just_labels=c(left="right",top="left"))

  })

  ## GridAnswer CA

  output$GridAnswer_CA <- renderPlot({
    selected_vars <- input$variables
    count_categories <- function(x) {
      table(factor(x, levels = c("++", "+", "-", "--", "DK", "無回答"), exclude = NULL))
    }

    # df の 1 列目から 20 列目の各列ごとにカテゴリを集計
    category_count_tbl <- data_for_plot() %>%
      #dplyr::summarise(across(selected_vars, ~ count_categories(.)))
      dplyr::reframe(across(selected_vars, ~ count_categories(.)))

    category_count_tbl %>% as.matrix() -> cat_tbl
    rownames(cat_tbl) <- c("++","+","-","--","DK","無回答")
    cat_tbl

    res.CA <- FactoMineR::CA(t(cat_tbl))

  })

  # Grid Mosaic 2 LK/DLK

  output$GridAnswer2_mosaic <- renderPlot({
    grid_ptn <- c("A","B","C","D","E","NA") # 仮のパターン
    selected_vars <- input$variables
    count_categories <- function(x) {
      table(factor(x, levels = grid_ptn,
                   exclude = NULL))
    }

    # df の 1 列目から 20 列目の各列ごとにカテゴリを集計
    category_count_tbl <- data_for_plot() %>%
      dplyr::reframe(across(selected_vars, ~ count_categories(.)))

    category_count_tbl %>% as.matrix() -> cat_tbl
    rownames(cat_tbl) <- grid_ptn
    cat_tbl

    rownames(t(cat_tbl)) -> rnames
    t(cat_tbl) %>% as.tibble() %>% dplyr::mutate(ID=rnames,IDn=1:length(rnames)) %>%
      #        mutate(Like=`++`+`+`) %>%
      dplyr::arrange(desc(A)) %>%
      dplyr::select(IDn) %>% unlist %>%
      setNames(NULL) -> order_vec

    #      t(cat_tbl)[order_vec,]
    t(cat_tbl) %>% vcd::mosaic(shade = TRUE,rot_labels = c(0, 0),
                               margins=c(left=9,top=5),just_labels=c(left="right",top="left"))

  })


  # GridAnswer2 CA
  output$GridAnswer2_CA <- renderPlot({
    grid_ptn <- c("A","B","C","D","E","NA") # 仮のパターン
    selected_vars <- input$variables
    count_categories <- function(x) {
      table(factor(x, levels = grid_ptn,#c("++", "+", "-", "--", "DK", "無回答"),
                   exclude = NULL))
    }

    # df の 1 列目から 20 列目の各列ごとにカテゴリを集計
    category_count_tbl <- data_for_plot() %>%
      dplyr::reframe(across(selected_vars, ~ count_categories(.)))

    category_count_tbl %>% as.matrix() -> cat_tbl
    rownames(cat_tbl) <- grid_ptn #c("++","+","-","--","DK","無回答")
    cat_tbl

    res.CA <- FactoMineR::CA(t(cat_tbl))

  })


  # Grid Mosaic Genenral

  output$GridAnswerG_mosaic <- renderPlot({
    req(input$variables)
    selected_vars <- input$variables
    vectors <- purrr::map(selected_vars, ~ {
      data_for_plot() %>% select(selected_vars) %>%
        dplyr::count(!!sym(.x)) %>%  # 選択した列ごとにカウント
        dplyr::pull(1)               # 最初の列のユニークな値を取得
    })

    union_all <- reduce(vectors, union)
    union_all <- ifelse(is.na(union_all), "NA", union_all)
    count_categories <- function(x) {
      table(factor(x, levels = union_all, exclude =NULL))
    }

    # df のselected_varsの各列ごとにカテゴリを集計
    category_count_tbl <- data_for_plot() %>%
      dplyr::reframe(across(selected_vars, ~ count_categories(.))) # summarize -> reframe

    category_count_tbl %>% as.matrix() -> cat_tbl
    rownames(cat_tbl) <- union_all #grid_ptn
    cat_tbl
    # cat_tbl の名前部分に NA が含まれていれば "NA" に変換
    names(cat_tbl) <- ifelse(is.na(names(cat_tbl)), "NA", names(cat_tbl))

    rownames(t(cat_tbl)) -> rnames
    t(cat_tbl) %>% as.tibble() %>% dplyr::mutate(ID=rnames,IDn=1:length(rnames)) %>% # ★t(cat_tbl) %>% as.tibble() でエラー
      dplyr::arrange(desc(union_all[1])) %>%
      dplyr::select(IDn) %>% unlist %>%
      setNames(NULL) -> order_vec

    t(cat_tbl)[order_vec,] %>%
      #t(cat_tbl) %>%
      vcd::mosaic(shade = TRUE,rot_labels = c(0, 0),
                  margins=c(left=12,top=5),just_labels=c(left="right",top="left"))

  })


  # GridAnswerG CA
  output$GridAnswerG_CA <- renderPlot({
    req(input$variables)
    selected_vars <- input$variables
    #browser()
    vectors <- purrr::map(selected_vars, ~ {
      data_for_plot() %>% dplyr::select(selected_vars) %>%
        dplyr::count(!!sym(.x)) %>%  # 選択した列ごとにカウント
        dplyr::pull(1)               # 最初の列のユニークな値を取得
    })

    union_all <- reduce(vectors, union)

    count_categories <- function(x) {
      table(factor(x, levels = union_all, exclude = NULL))
    }

    # df のselected_varsの各列ごとにカテゴリを集計
    category_count_tbl <- data_for_plot() %>%
      #dplyr::summarise(across(selected_vars, ~ count_categories(.)))
      dplyr::reframe(across(selected_vars, ~ count_categories(.)))

    category_count_tbl %>% as.matrix() -> cat_tbl
    rownames(cat_tbl) <- union_all #grid_ptn
    cat_tbl

    res.CA <- FactoMineR::CA(t(cat_tbl))

  })


  # タイムスタンプを取得
  output$timestamp <- renderText({
    file_info <- file.info("app.R")  # ファイル情報を取得
    timestamp <- file_info$mtime     # 最終更新日時
    paste("app.Rの最終更新:", format(timestamp, "%Y-%m-%d %H:%M:%S"))
  })

  # 度数分布表
  output$simple_table <- DT::renderDataTable({ #renderTable({#
    table(data_for_plot()[[input$variables[1]]]) -> tmp #select_input_data_for_hist]) -> tmp
    round(100*prop.table(tmp),1) -> tmp2
    data.frame(tmp,rate=tmp2)[,c(1,2,4)]
  })

  output$simple_table2 <- DT::renderDataTable({ #renderTable({#
    table(data_for_plot()[[input$select_input_data_for_hist]]) -> tmp
    round(100*prop.table(tmp),1) -> tmp2
    data.frame(tmp,rate=tmp2)[,c(1,2,4)]
  })


  output$table_for_plot <- DT::renderDataTable({
    data_for_plot() %>% select(input$variables)
  })
}

#-------------------------------------------------------------------------------
# Run the application
#
shinyApp(ui = ui, server = server)
#}
