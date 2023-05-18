library(ComplexHeatmap)
library(dendextend)
library(dendsort)


mat1 <- read.csv("data.csv")
mat1 <- mat1[, -1] 
mat1 <- as.matrix(mat1) 

Heatmap(mat1, row_split = split)


circos.clear()

library(circlize) 
col_fun1 = colorRamp2(c(-2, 0, 2), c("blue", "white", "red"))
circos.heatmap(mat1, split = split, col = col_fun1)

circos.clear()

circos.heatmap(mat1, col = col_fun1)

circos.clear()

circos.par(start.degree = 90, gap.degree = 10)
circos.heatmap(mat1, split = split, col = col_fun1, track.height = 0.4, 
    bg.border = "green", bg.lwd = 2, bg.lty = 2, show.sector.labels = TRUE)

circos.clear()

# note since circos.clear() was called in the previous plot,
# now the layout starts from theta = 0 (the first sector is 'e')
circos.heatmap(mat1, split = factor(split, levels = c("e", "d", "c", "b", "a")), 
    col = col_fun1, show.sector.labels = TRUE)

circos.clear()

circos.heatmap(mat1, split = split, col = col_fun1, dend.side = "inside")
circos.clear()
circos.heatmap(mat1, split = split, col = col_fun1, dend.side = "outside")
circos.clear()

circos.heatmap(mat1, split = split, col = col_fun1, rownames.side = "inside")
circos.clear()
text(0, 0, 'rownames.side = "inside"')
circos.heatmap(mat1, split = split, col = col_fun1, rownames.side = "outside")
circos.clear()
text(0, 0, 'rownames.side = "outside"')

circos.heatmap(mat1, split = split, col = col_fun1, dend.side = "inside", 
    rownames.side = "outside")
circos.clear()
circos.heatmap(mat1, split = split, col = col_fun1, dend.side = "outside", 
    rownames.side = "inside")
circos.clear()

circos.heatmap(mat1, split = split, col = col_fun1, rownames.side = "outside",
    rownames.col = 1:nrow(mat1) %% 10 + 1,
    rownames.cex = runif(nrow(mat1), min = 0.3, max = 2),
    rownames.font = 1:nrow(mat1) %% 4 + 1)

circos.clear()


circos.heatmap(mat1, split = split, cluster = FALSE, col = col_fun1)
circos.clear()

column_od = hclust(dist(t(mat1)))$order
circos.heatmap(mat1[, column_od], col = col_fun1)
circos.clear()

par(mfrow = c(1, 2))
hc = hclust(dist(mat1), method = "single")
circos.heatmap(mat1, cluster = hc, col = col_fun1, dend.side = "inside")
circos.clear()

circos.heatmap(mat1, cluster = hc, split = 2, col = col_fun1, dend.side = "inside")

circos.clear()
par(mfrow = c(1, 1))

library(dendsort)
circos.heatmap(mat1, split = split, col = col_fun1, dend.side = "inside",
    dend.callback = function(dend, m, si) {
        dendsort(dend)
    }
)
circos.clear()

library(dendextend)
dend_col = structure(1:5, names = letters[1:5])
circos.heatmap(mat1, split = split, col = col_fun1, dend.side = "inside",
    dend.track.height = 0.2,
    dend.callback = function(dend, m, si) {
        # when k = 1, it renders one same color for the whole dendrogram
        color_branches(dend, k = 1, col = dend_col[si])
    }
)

circos.clear()

circos.heatmap(mat1, col = col_fun1, dend.side = "inside",
    dend.track.height = 0.2,
    dend.callback = function(dend, m, si) {
        color_branches(dend, k = 4, col = 2:5)
    }
)

circos.clear()


mat2 = mat1[sample(100, 100), ] # randomly permute mat1 by rows
col_fun2 = colorRamp2(c(-2, 0, 2), c("green", "white", "red"))

circos.heatmap(mat1, split = split, col = col_fun1, dend.side = "outside")
circos.heatmap(mat2, col = col_fun2)


circos.clear()

circos.heatmap(mat2, split = split, col = col_fun2, dend.side = "outside")
circos.heatmap(mat1, col = col_fun1)

circos.clear()

circos.heatmap.initialize(mat1, split = split)
circos.heatmap(mat2, col = col_fun2, dend.side = "outside")
circos.heatmap(mat1, col = col_fun1)

circos.clear()

circos.heatmap.initialize(mat1, split = split)
circos.heatmap(mat1[, 1:5], col = col_fun1)
circos.heatmap(mat1[, 6:10], col = col_fun1)

circos.clear()

circos.heatmap(mat1, split = split, col = col_fun1)
row_mean = rowMeans(mat1[, 1:5])
circos.track(ylim = range(row_mean), panel.fun = function(x, y) {
    y = row_mean[CELL_META$subset]
    y = y[CELL_META$row_order]
    circos.lines(CELL_META$cell.xlim, c(0, 0), lty = 2, col = "grey")
    circos.points(seq_along(y) - 0.5, y, col = ifelse(y > 0, "red", "blue"))
}, cell.padding = c(0.02, 0, 0.02, 0))


circos.clear()

circos.heatmap.initialize(mat1, split = split)
# This is the same as the previous example
circos.track(ylim = range(row_mean), panel.fun = function(x, y) {
    y = row_mean[CELL_META$subset]
    y = y[CELL_META$row_order]
    circos.lines(CELL_META$cell.xlim, c(0, 0), lty = 2, col = "grey")
    circos.points(seq_along(y) - 0.5, y, col = ifelse(y > 0, "red", "blue"))
}, cell.padding = c(0.02, 0, 0.02, 0))
circos.heatmap(mat1, col = col_fun1) # no need to specify 'split' here


circos.clear()


circos.heatmap(mat1, split = split, col = col_fun1)
circos.track(ylim = range(mat1), panel.fun = function(x, y) {
    m = mat1[CELL_META$subset, 1:5, drop = FALSE]
    m = m[CELL_META$row_order, , drop = FALSE]
    n = nrow(m)
    # circos.boxplot is applied on matrix columns, so here we transpose it.
    circos.boxplot(t(m), pos = 1:n - 0.5, pch = 16, cex = 0.3)
    circos.lines(CELL_META$cell.xlim, c(0, 0), lty = 2, col = "grey")
}, cell.padding = c(0.02, 0, 0.02, 0))

circos.clear()

circos.heatmap(mat1, split = split, col = col_fun1)
circos.track(track.index = get.current.track.index(), panel.fun = function(x, y) {
    circos.text(CELL_META$xcenter, CELL_META$cell.ylim[2] + convert_y(2, "mm"), 
        paste0("this is group ", CELL_META$sector.index),
        facing = "bending.inside", cex = 0.8,
        adj = c(0.5, 0), niceFacing = TRUE)
}, bg.border = NA)


circos.clear()

circos.par(gap.after = c(2, 2, 2, 2, 10))
circos.heatmap(mat1, split = split, col = col_fun1, track.height = 0.4)
circos.track(track.index = get.current.track.index(), panel.fun = function(x, y) {
    if(CELL_META$sector.numeric.index == 5) { # the last sector
        cn = colnames(mat1)
        n = length(cn)
        circos.text(rep(CELL_META$cell.xlim[2], n) + convert_x(1, "mm"), 
            n - 1:n + 0.5, cn, 
            cex = 0.5, adj = c(0, 0.5), facing = "inside")
    }
}, bg.border = NA)


circos.clear()


circos.par(gap.after = c(2, 2, 2, 2, 10))
circos.heatmap(mat1, split = split, col = col_fun1, track.height = 0.4)
circos.track(track.index = get.current.track.index(), panel.fun = function(x, y) {
    if(CELL_META$sector.numeric.index == 5) { # the last sector
        circos.rect(CELL_META$cell.xlim[2] + convert_x(1, "mm"), 0,
                    CELL_META$cell.xlim[2] + convert_x(5, "mm"), 5,
                    col = "orange", border = NA)
        circos.text(CELL_META$cell.xlim[2] + convert_x(3, "mm"), 2.5,
                    "group 1", cex = 0.5, facing = "clockwise")

        circos.rect(CELL_META$cell.xlim[2] + convert_x(1, "mm"), 5,
                    CELL_META$cell.xlim[2] + convert_x(5, "mm"), 10,
                    col = "pink", border = NA)
        circos.text(CELL_META$cell.xlim[2] + convert_x(3, "mm"), 7.5,
                    "group 2", cex = 0.5, facing = "clockwise")
    }
}, bg.border = NA)

circos.clear()

circos.heatmap(mat1, split = split, col = col_fun1)
circos.clear()

library(ComplexHeatmap)
lgd = Legend(title = "mat1", col_fun = col_fun1)
grid.draw(lgd)


