######################################################################################################################################################################################
#������:mnplots
#��������:��uncles�������صľ������������ݽ��мӹ����Ӷ�����ͼ��
#����:	
#   unclesRusult:uncles�������صľ�������������
#               X:datasetss
#               B:a list of partitions; B[[i]] ��һ����ֵ������M�д���M gene�������ǵ�ʵ�����ǲ��Ǵ���channel?��
#                 K����K��clusters. B[[i]][j,k]  is the binary membership (either 1 or 0) of the (j)th gene in the (k)th cluster as per the (i)th partition. 
#                 it will be a 4D list array of binary partitions with the dimensions of:
#                       (T)x(NBP1)x(NBP2)x(NKs)         #geneʵ���е�һ��trail�����ǵ�һ��trail��һ����˼ô?
#                        (T) is the number of the CoPaM ?nal trials(�м���trail�����Ǽ�)
#                        (NBP1) ����ֵ��ʱ�ĦĲ����ĸ��������ǳ��Ե���ֵ�ĸ�����is the number of the different values of the parameter of the binarisation technique.....
#                        (NBP2) is 1 if the UNCLEStypeis"A",andisthenumberofthedifferentvaluesoftheparameter of the negative binarisation technique if UNCLES type is "B" #ʲô��negative binarisation technuque
#                        (NKs) is the number of the different numbers of clusters (K values)   
#               GDM: M rows representing M genes and L columns representing L datasets. #����genʵ���е�gene �� dataset��ʲô����???
#                     indicates that the corresponding gene is found in the corresponding dataset
#               params��type: The type of UNCLES, ��A�� or ��B��. Default: ��A��
#                       setsP:For UNCLES type
#                       setsN: For UNCLES type 
#                       wsets: For L datasets, this is a vector of L numeric values representing the relative weights of the datasets
#    MCs:ѡ����Ļ�ͼ�Ĵ�������Ӧ�������е�  the number of iterations, as in each iteration one cluster is selected
#    corner:�ο��㣬����������Ծ��룬�������Խ���ĵ�������õĵ㣬ˮƽ��������ڵ���ɢ�ȣ���ֱ������صĴ�С
#           ����corner������õĲ�ͬ��ƫ�ң���ѡ���wider cluster��ƫ��ѡ��tighter clusters
#    removedtype��   overlap???
#                       abs / perc
#    removedval������overlap֮��ѡ���й�ϵ
#                abs:����0����  
#                perc:����percentage
#                ��removedtype���������������ϵͳ��plot������ĳһά�ȵ����ݽ���ɸѡ���Ƚ���removedtype������Ȼ��ó���ֵ��removedval���бȽϣ�Ȼ��ó�T/F,F��Ӧ��index�����ử����              
#	Vmse:���������ķ��ز����й�
#	mseCache:����
#	doplot:�����Ƿ�ͼ����Ȼȱʡ����FALSE����һ������TRUE�����õ�ͼ��
#	subplotdim:�ͻ�ͼ�йأ�����Ҫ
#	subplotind:������û���ù��������
#	minimiseDistance:����Vds��ȡ�������С
#����ֵ:B: logical[128*10] ,ά��128����128��channel��10����MCs����Ҫѡ���10���أ�ȫ������T/F���������channel�Ƿ���������м�
#	C: double [10*8] ������ÿһ�еĺ����ӦHeader
#	Header:��������ÿһ�е�����,"Index" "K" "t" "k" "Mc" "mse*" "dist" "d"
#	Vmse:double 440 mseӦ���Ƿ�������壨�ڴ�����Ҳȷʵ�ҵ��˷���ļ��㣩�����ǲ�֪��ΪʲôҪ��V��ά��440=4*11��4����4 8 12 16 ���־��������11�Ƕ�ֵ������һ����ֵ����uncles�����о��Ѿ�ȷ������һ���Ƚ����������
#	mseCache:��Vmse����ֵһ����ֻ������������� double[440*1] ÿһ��Ԫ�ش���һ��partition
#��д�ߣ����
#ʱ�䣺2019/4/23
#######################################################################################################################################################################################


mnplots <- function(unclesResult, MCs = 10, corner = c(0, 1),			#MCsΪʲôҪȱʡ����10���������corner��������������Ӱ�쵽���Ĳ���
    removedtype = 'abs', removedval = 1,
    Vmse = numeric(), mseCache = numeric(), doplot = FALSE, subplotdim = numeric(),
    subplotind = 1:MCs, minimiseDistance = TRUE) {

    ####################### Some parameters' fixing #######################	#����������ݽ���һЩ�������������ǵ�����ֻ��unclesResult�������������еĲ������ɴ���ȱʡ����
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

        if (isnullorempty(GDM)) {						#��GDM��ֵ��ȫ��true
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
            Xtype = rep(Xtype, length(X))					#X��һ����λ���ȵ�list����������ľ���Ϊʲô��25*40
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
        }								##tightness_param=[0, 0.1,  0.2  , ....,1] ,��Ӧ����uncles�����ж�ֵ����������ֵ
    }

    ####################### Initialisation #######################
    if (TRUE) {
        if (length(dim(B)) == 4) {					#B��һ����ά��array��uncles��һ�δ�������ȡ�м���ά���в��� 1 11 1 4
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
            K[n] = ncol(B[[n]])						#B��һ��list������4 8 12 16 ��11����ÿһ��ά�Ȳ�һ��
        }

        # Total number of all clusters
        NN = sum(K)

        VMc = rep(0, NN)							#��һ��������Ӧ�����index
        VKs = rep(0, NN)
        Vks = rep(0, NN)
        Vds = rep(0, NN)
        Vd1s = rep(0, NN)
        Vd2s = rep(0, NN)
        Vns = rep(0, NN)
        Vtrial = rep(0, NN)
        Bs = matrix(FALSE, nrow(B[[1]]), MCs)					#dim(B)=128*10  ȫ��False
    }

    ####################### Fill Vmse if not provided #######################
    if (isempty(mseCache) && isempty(Vmse)) {
        mseCache = matrix(0, NN, length(X))					#440*1 ȫ��0
        for (l in 1:length(X)) {
            nn = 0
            if (tolower(Xtype[[l]]) == "rds") {					#����ʱֵΪ"data"
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
                        mseCache[nn, l] = mseclusters(Xtmp, B[[n]][GDM[, l], k], FALSE)$mse	#mseCache��440*1��Ȼ��mseclusters
                        if (is.na(mseCache[nn, l])) {						#��ȥ�����mse(����)�����ѭ���������440
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
            VMc[nn] = sum(B[[n]][, k])		#B[[n]]��һ������ÿһ����128��Ȼ��k��ӦK��K֮ǰ�����444 8888 1212121 1616166,VMc����ָÿһ�е�TRUE�ĸ���
            VKs[nn] = K[n]			#���Խ���һ�е���ͣ�Ȼ��VKs����4444 8888....
            Vks[nn] = k
            Vns[nn] = n
            Vtrial[nn] = (n - 1) %% Ntrials + 1
            if (toupper(type) == "A") {
                Vd1s[nn] = tightness_param[floor((n - 1) / Ntrials) %% dim1 + 1]	#��11Ϊ���ڣ�40*11=440��11������0-1
            } else if (toupper(type) == "B") {
                Vd1s[nn] = tightness_param[[1]][floor((n - 1) / Ntrials) %% dim1 + 1]
                Vd2s[nn] = tightness_param[[2]][floor((floor((n - 1) / Ntrials) %% (dim1 * dim2)) / dim1) + 1]
            }
        }
    }

    ####################### Find distances #######################
    maxx = quantile(Vmse, 1)					#�����λ��
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
    vecs = cbind((Vmse - minx) / (maxx - minx), (log10(VMc) - miny) / (maxy - miny))	#��һ��  lg֮���һ��
    Vds = as.vector(pdist::pdist(corner, vecs)@dist)				    #Partitioned Distances������

    ####################### Find the best clusters and plot them #######################
    if (doplot) {
        if (isnullorempty(subplotdim)) {
            subplotdim = closestToSquareFactors(MCs)					#�õ�factors�������ӣ�Ȼ�󰴴�С������������sqart(n)��С��һ��
        }

        dev.new()									                    #������ͼ��mfrow������������
        par(mfrow = c(subplotdim[1], subplotdim[2]))
    }
    
    I = rep(TRUE, NN)
    I[VMc <= 0] = FALSE

    if (toupper(type) == "A") {
        C = matrix(, MCs, 8)
        Header = c('Index', 'K', 't', 'k', 'Mc', 'mse*', 'dist', 'd')			#����C�ĸ����е�����
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
        C[mi, 2] = VKs[C[mi, 1]]							#��C�ı�ǩ���ƶϱ�������
        C[mi, 3] = Vtrial[C[mi, 1]]							#K��B�����ͣ�ȫ��4444 8888��k��column 1234 1234 ... 1-8  1-16
        C[mi, 4] = Vks[C[mi, 1]]
        C[mi, 5] = VMc[C[mi, 1]]
        C[mi, 6] = Vmse[C[mi, 1]]
        C[mi, 8] = Vd1s[C[mi, 1]]							#��Ӧd����ʲô��˼��					
        if (toupper(type) == "B") {
            C[mi, 9] = Vd2s[C[mi, 1]]
        }

        # Remove similar
        nn1 = 0
        Ir = rep(FALSE, length(Vmse))
        B_best = B[[Vns[C[mi, 1]]]][, C[mi, 4]]						#VnsҲ�Ǻ�K��ֻ࣬������1111 2222 3333 ...44 44 44
        Bs[, mi] = B_best								#B��44��list��ÿһ����128*4��Ȼ����4�֣�4 8 12 16��ÿ��11��
        for (n in 1:N) {								#B_best��ȡindex��k(column)
            for (k in 1:K[n]) {								
                nn1 = nn1 + 1
                if (I[nn1]) {
                    if (tolower(removedtype) == "perc") {
                        if (sum(B[[n]][, k] & B_best) / min(sum(B[[n]][, ]), C[mi, 5]) >= removedval) {
                            I[nn1] = FALSE
                            Ir[nn1] = TRUE
                        }
                    } else if (tolower(removedtype) == "abs") {
                        if (sum(B[[n]][, k] & B_best) >= removedval) {			#����removedval�����Ƿ�Ҫ�Ƴ�
                            I[nn1] = FALSE
                            Ir[nn1] = TRUE
                        }
                    }
                }
            }
        }

        if (doplot) {									#����Ļ�ͼ�Ĳ���
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