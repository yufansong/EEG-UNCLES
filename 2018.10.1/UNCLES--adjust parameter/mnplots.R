######################################################################################################################################################################################
#函数名:mnplots
#函数功能:对uncles函数返回的经过处理的数据进行加工，从而画出图像
#参数:	
#   unclesRusult:uncles函数返回的经过处理的数据
#               X:datasetss
#               B:a list of partitions; B[[i]] 是一个二值化矩阵，M行代表M gene（在咱们的实验中是不是代表channel?）
#                 K代表K个clusters. B[[i]][j,k]  is the binary membership (either 1 or 0) of the (j)th gene in the (k)th cluster as per the (i)th partition. 
#                 it will be a 4D list array of binary partitions with the dimensions of:
#                       (T)x(NBP1)x(NBP2)x(NKs)         #gene实验中的一个trail和咱们的一个trail是一个意思么?
#                        (T) is the number of the CoPaM ?nal trials(有几个trail，就是几)
#                        (NBP1) （二值化时的δ参数的个数，就是尝试的阈值的个数）is the number of the different values of the parameter of the binarisation technique.....
#                        (NBP2) is 1 if the UNCLEStypeis"A",andisthenumberofthedifferentvaluesoftheparameter of the negative binarisation technique if UNCLES type is "B" #什么是negative binarisation technuque
#                        (NKs) is the number of the different numbers of clusters (K values)   
#               GDM: M rows representing M genes and L columns representing L datasets. #他们gen实验中的gene 和 dataset有什么区别???
#                     indicates that the corresponding gene is found in the corresponding dataset
#               params：type: The type of UNCLES, ’A’ or ’B’. Default: ’A’
#                       setsP:For UNCLES type
#                       setsN: For UNCLES type 
#                       wsets: For L datasets, this is a vector of L numeric values representing the relative weights of the datasets
#    MCs:选择出的画图的簇数，对应于论文中的  the number of iterations, as in each iteration one cluster is selected
#    corner:参考点，用来计算相对距离，靠这个点越近的点代表更好的点，水平轴代表簇内的离散度，竖直轴代表簇的大小
#           根据corner点的设置的不同，偏右，会选择更wider cluster，偏左选择tighter clusters
#    removedtype：   overlap???
#                       abs / perc
#    removedval：和在overlap之间选择有关系
#                abs:大于0即可  
#                perc:大于percentage
#                和removedtype结合起来，对最后的系统的plot函数的某一维度的数据进行筛选，先进行removedtype操作，然后得出的值与removedval进行比较，然后得出T/F,F对应的index将不会画出来              
#	Vmse:方差，和输出的返回参数有关
#	mseCache:方差
#	doplot:控制是否画图，虽然缺省的是FALSE，但一般输入TRUE，来得到图像
#	subplotdim:和画图有关，不重要
#	subplotind:代码中没有用过，很奇怪
#	minimiseDistance:控制Vds中取最大还是最小
#返回值:B: logical[128*10] ,维数128代表128个channel，10代表MCs的是要选择出10个簇，全部都是T/F，代表这个channel是否在这个簇中间
#	C: double [10*8] 矩阵中每一列的含义对应Header
#	Header:代表矩阵每一列的意义,"Index" "K" "t" "k" "Mc" "mse*" "dist" "d"
#	Vmse:double 440 mse应该是方差的意义（在代码中也确实找到了方差的计算），但是不知道为什么要加V，维数440=4*11，4代表4 8 12 16 四种聚类情况，11是二值化的是一个阈值，在uncles函数中就已经确定，是一个比较特殊的数字
#	mseCache:和Vmse的数值一样，只不过变成了向量 double[440*1] 每一个元素代表一个partition
#编写者：宋宇凡
#时间：2019/4/23
#######################################################################################################################################################################################


mnplots <- function(unclesResult, MCs = 10, corner = c(0, 1),			#MCs为什么要缺省等于10，还有这个corner，这两个参数会影响到最后的操作
    removedtype = 'abs', removedval = 1,
    Vmse = numeric(), mseCache = numeric(), doplot = FALSE, subplotdim = numeric(),
    subplotind = 1:MCs, minimiseDistance = TRUE) {

    ####################### Some parameters' fixing #######################	#对输入的数据进行一些操作，由于我们的输入只是unclesResult参数，其余所有的参数均由代码缺省给出
    if (TRUE) {
        X = unclesResult$X
        B = unclesResult$B
        GDM = unclesResult$GDM
        type = unclesResult$params$type
        setsP = unclesResult$params$setsP
        setsN = unclesResult$params$setsN
        wsets = unclesResult$params$wsets
        Xtype = unclesResult$params$Xtype
        tightness_param = unclesResult$params$binarisation_param

        if (isnullorempty(X) || isnullorempty(B)) {
            stop("the unclesResult has to include X and B at least")
        }

        if (!is.list(X) || is.data.frame(X)) {
            X = list(X)
        }
        if (!is.list(B) || is.data.frame(B)) {
            B = list(B)
        }

        if (isnullorempty(type)) {
            type = "A"
        }

        if (isnullorempty(setsP)) {
            if (toupper(type) == "A" || length(X) == 1) {
                setsP = 1:length(X)
            } else if (toupper(type) == "B") {
                setsP = 1:(ceiling(length(X) / 2))
            }
        }

        if (isnullorempty(setsN)) {
            if (toupper(type) == "A" || length(X) == 1) {
                setsN = numeric()
            } else if (toupper(type) == "B") {
                setsN = seq(from = ceiling(length(X) / 2) + 1, to = length(X))
            }
        }

        if (isnullorempty(GDM)) {						#给GDM赋值，全部true
            GDM = matrix(TRUE, nrow(X[[1]]), length(c(setsP, setsN)))
        } else {
            GDM = GDM[, c(setsP, setsN)]
        }

        if (isnullorempty(wsets)) {
            wsets = rep(1, length(X))
        }

        if (isnullorempty(Xtype)) {
            Xtype = "data"
        }
        if (!is.list(Xtype)) {
            Xtype = as.list(Xtype)
        }
        if (length(Xtype) == 1) {
            Xtype = rep(Xtype, length(X))					#X是一个单位长度的list，但是里面的矩阵为什么是25*40
        }

        if (isnullorempty(tightness_param)) {
            if (toupper(type) == "A") {
                if (!is.vector(B) && length(dim(B)) > 1 && dim(B)[2] > 1) {
                    tightness_param = seq(0, 1, by = 1 / (dim(B)[2] - 1))
                } else {
                    tightness_param = 0
                }
            } else if (toupper(type) == "B") {
                tightness_param = list();
                if (!is.vector(B) && length(dim(B)) > 1 && dim(B)[2] > 1) {
                    tightness_param[[1]] = seq(0, 1, by = 1 / (dim(B)[2] - 1))
                } else {
                    tightness_param[[1]] = 0
                }
                if (!is.vector(B) && length(dim(B)) > 2 && dim(B)[3] > 1) {
                    tightness_param[[2]] = seq(0, 1, by = 1 / (dim(B)[3] - 1))
                } else {
                    tightness_param[[2]] = 0
                }
            }
        }								##tightness_param=[0, 0.1,  0.2  , ....,1] ,对应的是uncles函数中二值化操作的阈值
    }

    ####################### Initialisation #######################
    if (TRUE) {
        if (length(dim(B)) == 4) {					#B是一个四维的array，uncles有一段代码里面取中间两维进行操作 1 11 1 4
            Ntrials = dim(B)[1]
        } else {
            Ntrials = 1
        }

        #B <- rep(B,1)
        N = length(B)							#length(B)=1*4*11*1=44
        K = rep(0, N)			

        if (!is.list(X)) {
            X = list(X)
        }

        for (n in 1:N) {
            K[n] = ncol(B[[n]])						#B是一个list，里面4 8 12 16 各11个，每一个维度不一样
        }

        # Total number of all clusters
        NN = sum(K)

        VMc = rep(0, NN)							#这一串参数对应后面的index
        VKs = rep(0, NN)
        Vks = rep(0, NN)
        Vds = rep(0, NN)
        Vd1s = rep(0, NN)
        Vd2s = rep(0, NN)
        Vns = rep(0, NN)
        Vtrial = rep(0, NN)
        Bs = matrix(FALSE, nrow(B[[1]]), MCs)					#dim(B)=128*10  全是False
    }

    ####################### Fill Vmse if not provided #######################
    if (isempty(mseCache) && isempty(Vmse)) {
        mseCache = matrix(0, NN, length(X))					#440*1 全是0
        for (l in 1:length(X)) {
            nn = 0
            if (tolower(Xtype[[l]]) == "rds") {					#运行时值为"data"
                Xtmp = readRDS(X[[l]])
            } else if (tolower(Xtype[[l]]) == "rdata") {
                stop("Loading datasets from RData files is not supported yet. Please contact the maintainer")
            } else {
                Xtmp = X[[l]]
            }
            for (n in 1:N) {
                for (k in 1:K[n]) {
                    nn = nn + 1
                    if (sum(B[[n]][, k]) > 0) {
                        mseCache[nn, l] = mseclusters(Xtmp, B[[n]][GDM[, l], k], FALSE)$mse	#mseCache是440*1，然后mseclusters
                        if (is.na(mseCache[nn, l])) {						#进去算的是mse(方差)，最后循环结果填满440
                            mseCache[nn, l] = 0
                        }
                    }
                }
            }
            rm(list = "Xtmp")
            sprintf("X %i MSE calculation done.", l)
        }
    }
    if (isempty(Vmse)) {
        if (toupper(type) == "A") {
            wsets = wsets[c(setsP, setsN)] / sum(wsets[c(setsP, setsN)])			#wsets=1 setsN-0  setsP=1 
            Vmse = as.vector(mseCache[, c(setsP, setsN)] %*% matrix(wsets))
        } else if (toupper(type) == "B") {
            wsetsP = wsets[setsP] / sum(wsets[setsP])
            wsetsN = wsets[setsN] / sum(wsets[setsN])
            Vmse = as.vector(mseCache[, setsP] %*% matrix(wsetsP) - mseCache[, setsN] %*% matrix(wsetsN))
        }
    }

    ####################### Fill vectors with data #######################
    nn = 0
    if (toupper(type) == "A") {
        dim1 = length(tightness_param)								#tightness_param=[0, 0.1,  0.2  , ....,1]
    } else if (toupper(type) == "B") {
        dim1 = length(tightness_param[[1]])
        dim2 = length(tightness_param[[2]])
    }
    for (n in 1:N) {
        for (k in 1:K[n]) {
            nn = nn + 1
            VMc[nn] = sum(B[[n]][, k])		#B[[n]]是一个矩阵，每一列是128，然后k对应K，K之前提过是444 8888 1212121 1616166,VMc就是指每一列的TRUE的个数
            VKs[nn] = K[n]			#所以进行一列的求和，然后VKs就是4444 8888....
            Vks[nn] = k
            Vns[nn] = n
            Vtrial[nn] = (n - 1) %% Ntrials + 1
            if (toupper(type) == "A") {
                Vd1s[nn] = tightness_param[floor((n - 1) / Ntrials) %% dim1 + 1]	#以11为周期，40*11=440，11代表从0-1
            } else if (toupper(type) == "B") {
                Vd1s[nn] = tightness_param[[1]][floor((n - 1) / Ntrials) %% dim1 + 1]
                Vd2s[nn] = tightness_param[[2]][floor((floor((n - 1) / Ntrials) %% (dim1 * dim2)) / dim1) + 1]
            }
        }
    }

    ####################### Find distances #######################
    maxx = quantile(Vmse, 1)					#计算分位数
    minx = quantile(Vmse, 0)
    maxy = log10(max(VMc))
    miny = 0
    if (isnullorempty(corner)) {
        if (minimiseDistance) {
            corner = c(0, 1)
        } else {
            corner = c(1, 0)
        }
    }
    vecs = cbind((Vmse - minx) / (maxx - minx), (log10(VMc) - miny) / (maxy - miny))	#归一化  lg之后归一化
    Vds = as.vector(pdist::pdist(corner, vecs)@dist)				    #Partitioned Distances？？？

    ####################### Find the best clusters and plot them #######################
    if (doplot) {
        if (isnullorempty(subplotdim)) {
            subplotdim = closestToSquareFactors(MCs)					#用到factors来找因子，然后按从小到大的输出举例sqart(n)最小的一组
        }

        dev.new()									                    #开启画图，mfrow是行优先排列
        par(mfrow = c(subplotdim[1], subplotdim[2]))
    }
    
    I = rep(TRUE, NN)
    I[VMc <= 0] = FALSE

    if (toupper(type) == "A") {
        C = matrix(, MCs, 8)
        Header = c('Index', 'K', 't', 'k', 'Mc', 'mse*', 'dist', 'd')			#矩阵C的各个列的意义
    } else if (toupper(type) == "B") {
        C = matrix(, MCs, 9)
        Header = c('Index', 'K', 't', 'k', 'Mc', 'mse*', 'dist', 'd+', 'd-')
    }

    for (mi in 1:MCs) {
        # Find best cluster and fill a C row
        if (minimiseDistance) {
            C[mi, 7] = min(Vds[I])
            C[mi, 1] = which(Vds == C[mi, 7])[1]
        } else {
            C[mi, 7] = max(Vds[I])
            C[mi, 1] = which(Vds == C[mi, 7])[1]
        }
        C[mi, 2] = VKs[C[mi, 1]]							#由C的标签来推断变量意义
        C[mi, 3] = Vtrial[C[mi, 1]]							#K是B的类型，全是4444 8888，k是column 1234 1234 ... 1-8  1-16
        C[mi, 4] = Vks[C[mi, 1]]
        C[mi, 5] = VMc[C[mi, 1]]
        C[mi, 6] = Vmse[C[mi, 1]]
        C[mi, 8] = Vd1s[C[mi, 1]]							#对应d，是什么意思？					
        if (toupper(type) == "B") {
            C[mi, 9] = Vd2s[C[mi, 1]]
        }

        # Remove similar
        nn1 = 0
        Ir = rep(FALSE, length(Vmse))
        B_best = B[[Vns[C[mi, 1]]]][, C[mi, 4]]						#Vns也是和K差不多，只不过是1111 2222 3333 ...44 44 44
        Bs[, mi] = B_best								#B是44个list，每一个是128*4，然后有4种，4 8 12 16，每种11个
        for (n in 1:N) {								#B_best是取index和k(column)
            for (k in 1:K[n]) {								
                nn1 = nn1 + 1
                if (I[nn1]) {
                    if (tolower(removedtype) == "perc") {
                        if (sum(B[[n]][, k] & B_best) / min(sum(B[[n]][, ]), C[mi, 5]) >= removedval) {
                            I[nn1] = FALSE
                            Ir[nn1] = TRUE
                        }
                    } else if (tolower(removedtype) == "abs") {
                        if (sum(B[[n]][, k] & B_best) >= removedval) {			#根据removedval计算是否要移除
                            I[nn1] = FALSE
                            Ir[nn1] = TRUE
                        }
                    }
                }
            }
        }

        if (doplot) {									#常规的绘图的操作
            # Scatter params
            marksizekept = 1
            marksizeremoved = 1
            marksizebest = 1.5

            # Scatter kept
            plot.default(Vmse[I], VMc[I], pch = 0, cex = marksizekept, col = "black",
                        xlim = c(quantile(Vmse, 0) - 0.02, quantile(Vmse, 1) + 0.02),
                        ylim = c(1, max(VMc) + 0.02), log = "y", main = sprintf("C%i", mi),
                        xaxt = "n", yaxt = "n", xlab = "", ylab = "", frame.plot = TRUE);

            # Scatter removed
            points(Vmse[Ir], VMc[Ir], pch = 8, cex = marksizeremoved, col = "red")

            # Scatter best
            points(C[mi, 6], C[mi, 5], pch = 21, cex = marksizebest, col = "black", bg = "blue")
        }

        if (sum(I) == 0) {
            break
        }
    }

    return(list(B = Bs, C = C, Header = Header, Vmse = Vmse, mseCache = mseCache));
}