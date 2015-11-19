require(DiagrammeR)
require(magrittr)
##require(webshot) #' Need to install via devtools 
require(htmltools)
require(exportwidget)


#Format numbers as short string with correct 0.0k/m/...
formatNum <- function(x, pc = FALSE) {
  signv <- sign(x)
  x <- abs(x)
  if (!(pc)) {
    x <- ifelse(x < 10, sprintf("%.2f", x), ifelse(x < 1000, sprintf("%.0f", x), 
          ifelse(x < 1e6, paste0(round(x/1e3),"k"),
          ifelse(x < 1e7, paste0(round(x/1e6,1),"m"), paste0(round(x/1e6,0), "m")))))
  } else {
    x <- paste0(round(x*100,1),"%")
  }
  paste0(ifelse(signv == 1, "+", "-"),x)
}

KPItree <- function(td, optional_title_suffix = "") {
    
    # function for creating visualisations of Customer metrics
    # If you know customer numbers then it will split out in to four levels

    names(td) %<>% toupper(.)
    
    #' td = "tree data"
    #' td should take the form:
    #' [1] "GROUP_1" "GROUP_2"
    #' [3] "YEAR"                "CUSTOMERS"           "TXNS"    "UNITS"
    #' [7] "TISP"
    
    #The tree will be named after the category and card_flag
    
    tree_title <- paste(as.character(td[[1,1]]),
                        as.character(td[[1,2]]),
                        as.character(optional_title_suffix), 
                        sep = "\n")
    
    #' #Create Colour Gradient -- used iwanthue web app to avoid the barf spectrum
    colr <- colorRamp(c("#D04C2A", "#FC733A", "#EBC341", "#70DC4C", "#4FB800"))
    
    tree_depth  <-
        if (is.na(td$CUSTOMERS) ||
            td$CUSTOMERS == 0 || is.null(td$CUSTOMERS)) {
            #We need three levels in our trees
            3
        } else {
            4
        }
    
    #' New Dataset construction
    
    if (tree_depth == 3) {
        td %<>% mutate(ATV = TISP / TXNS,
                       AIV = TISP / UNITS,
                       ATU = UNITS / TXNS) %>%
            arrange(YEAR) %>%
            select(TISP, ATV, TXNS, ATU, AIV)
    } else if (tree_depth == 4) {
        td %<>%
            mutate(
                FREQ = TXNS / CUSTOMERS,
                ACV = TISP / CUSTOMERS,
                ATV = TISP / TXNS,
                AIV = TISP / UNITS,
                ATU = UNITS / TXNS
            ) %>%
            arrange(YEAR) %>%
            select(TISP, ACV, CUSTOMERS, ATV, FREQ, ATU, AIV)
    } else {
        stop("Wrong number of levels specified in trees")
    }
    
    # Compute YOY growth in ABS and % terms
    # td <- td[order(td$YEAR),-c(1:5)]
    td <- rbind(td, td[2, ] - td[1, ])
    td <- rbind(td, td[3, ] / td[1, ])
    
    #Humanise numbers
    absvf <- formatNum(as.numeric(as.vector(td[3,])))
    pv <- as.numeric(as.vector(td[4,]))
    pvf <- formatNum(pv, pc = TRUE)
    
    #Box colours
    col.tform <- pmax(pmin((pv + 0.1) / 0.2, 1), 0)
    col.tform <-
        ifelse(col.tform <= 0.6 & col.tform >= 0.4, 0.5, col.tform)
    cols <-
        as.character(rgb(colr(col.tform), max = 255)) #[c(4, 8, 7, 5, 6, 9, 1)]
    
    # Create node text
    if (tree_depth == 3) {
        tree.dat <- data.frame(var = pv,
                               tvar = pvf,
                               tabs = absvf)
        metrics <-
            c("TISP", "ATV", "Baskets", "ATU", "AIV") #colnames(td)[c(4,8,1,7,9,5,6)]
        pad <-
            10 - nchar(as.character(tree.dat$tabs)) - nchar(as.character(tree.dat$tvar))
        pad <-
            unlist(lapply(pad, function(x)
                ifelse(x > 0, rep(" ", 2 * x), "")))
    } else if (tree_depth == 4) {
        tree.dat <- data.frame(var = pv,
                               tvar = pvf,
                               tabs = absvf)
        metrics <-
            c("TISP", "ACV", "Customers", "ATV", "Freq", "ATU", "AIV") #colnames(td)[c(4,8,1,7,9,5,6)]
        pad <-
            10 - nchar(as.character(tree.dat$tabs)) - nchar(as.character(tree.dat$tvar))
        pad <-
            unlist(lapply(pad, function(x)
                ifelse(x > 0, rep(" ", 2 * x), "")))
    }
    
    # Create tree structure - nodes and edges
    
    nodes = ifelse(tree_depth == 3, 5, 7)
    
    if (tree_depth == 3){
            node_names <- c("Title", "TISP", "ATV", "Baskets", "ATU", "AIV")
    } else {
          node_names <- c("Title","TISP","ACV","Customers","ATV","Freq","ATU","AIV")
    }
    
    tree_nodes <- create_nodes(
        nodes = node_names,
        label = c(
            tree_title,
            paste0(metrics, "\n", pad, tree.dat$tabs, "  ", tree.dat$tvar, pad)
        ),
        shape = c("none", rep("box", nodes)),
        fontname = "Helvetica",
        style = c("", rep("rounded, filled", nodes)),
        fillcolor = c("", cols)
    )
    
    if (tree_depth == 3){
        tree_edges <- create_edges( 
        from = c("TISP", "TISP", "ATV", "ATV") ,
        to = c("ATV", "Baskets", "ATU", "AIV"),
        dir = "none"
        )
    } else{
        tree_edges <- create_edges(
            from = c("TISP", "TISP", "ACV", "ACV",  "ATV", "ATV"),
            to =   c("ACV", "Customers", "ATV", "Freq", "ATU", "AIV"),
            dir = "none"
        )
    }

    trees <- create_graph(nodes_df = tree_nodes,
                          edges_df = tree_edges)
    
    return(trees)
    
}

data <- read.csv("1. KPI Data.csv")

data %<>% filter(is.na(SALES_PLAN_YEAR_A)) %>% 
    arrange(GROUP_1, GROUP_2) %>% 
    .[,3:9]

# Plot tree in Viewer
KPItree(data[1:2,]) %>% render_graph() 

#' Working example - Well you have to construct data yourself
#' This will render the tree and put a PNG in the root of your project

for(i in seq(1, nrow(data), 2)) {
    title <- paste(as.character(data[[i, 1]]),
                   as.character(data[[i + 1, 2]]), sep = "_") %>% 
        sub(" ", "_",.)
    KPItree(data[i:(i + 1),]) %>% render_graph() %>% html_print(tagList(., export_widget())) %>%
        normalizePath(., winslash = "/") %>%
        gsub(x = .,
             pattern = ":/",
             replacement = "://") %>%
        paste0("file:///", .) %>%
        webshot(file = paste0(title, ".png"), delay = 0.5)
}
