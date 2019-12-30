#加载包
library("dplyr")
library("tidyr")
library("jsonlite")
library("purrr")
library("RCurl")
library("dplyr")
library("tidyr")
library("tibble")
library("stringr")
library(tidyverse)
library(stringr)
library(KEGGREST)
library(AnnotationForge)
library(tibble)
library(stringr)
library(KEGGREST)
library(readr)
library(clusterProfiler)
library(DOSE)
library(purrr)
library(tidyverse)
library(clusterProfiler)
library(AnnotationHub)
options(stringsAsFactors = F)
barley_go <- read.table("Barley.txt", header = T, stringsAsFactors = F, check.names = F)
phy_go <- read.table("Phy.txt", header = T, stringsAsFactors = F, check.names = F, sep = "\t")

total_go <- dplyr::full_join(phy_go,barley_go,by="GeneID")

barley_go[barley_go=="none"] <- NA
Hor_go <- na.omit(barley_go)
#去除所有列均相同的重复
uniq_go <- total_go[!duplicated(total_go),]
uniq_go[uniq_go=="none"] <- NA 
uniq_go[uniq_go==""] <- NA

GO1 <- tidyr::unite(uniq_go, "GO_GO-IDs","GO","GO-IDs",sep = ",")
write.table(GO1,file = "GO1.txt",sep = "\t",quote = F,row.names = F)
GO2 <- read.table("GO2.txt", header = T, stringsAsFactors = F, check.names = F)
GO2 <- GO2[!duplicated(GO2),]
GO_final <- na.omit(GO2)
###KO
KO1 <- read.table("Phy_KO.txt", header = T, stringsAsFactors = F, check.names = F, sep = "\t")
KO1[KO1==""] <- NA
KO2 <- na.omit(KO1)
KO_final <- KO2[!duplicated(KO2),]
#GeneID与GO号对应的信息
gene_go <- data.frame(GID = GO_final$GeneID,
                      GO = GO_final$`GO_GO-IDs`,
                      EVIDENCE = rep("IEA", length(GO_final$GeneID)))

gene_ko <- data.frame(GID = KO_final$GeneID,
                      Ko = KO_final$KO )

barley_info <- read.table("barley_anno.txt", 
                          header = T, 
                          stringsAsFactors = F, 
                          check.names = F,
                          sep = "\t")
gene_info0 <- barley_info %>%
  dplyr::select(GID=GeneID, GENENAME = description) %>% na.omit()
gene_info <- gene_info0[!duplicated(gene_info0),]
#得到pathway2name,ko2pathway

#Rscript parse_kegg_json.R

#利用KO将gene与pathway联系起来，然后挑出query_name与pathway注释信息
load(file = "kegg_info.RData")
gene_pathway <- dplyr::left_join(gene_ko, ko2pathway, by = "Ko") %>% 
  dplyr::select(GID, Pathway) %>% na.omit()

#制作自己得orgdb
# 查询物种的Taxonomy，例如要查barley
# https://www.ncbi.nlm.nih.gov/taxonomy/?term=barley

tax_id = "4513"
genus = "Hordeum" 
species = "vulgare"
gene_info <- gene_info[!duplicated(gene_info),]
gene_go <- gene_go[!duplicated(gene_go),]
gene_ko <- gene_ko[!duplicated(gene_ko),]
gene_pathway <- gene_pathway[!duplicated(gene_pathway),]
makeOrgPackage(gene_info=gene_info,
               go=gene_go,
               ko=gene_ko,
               pathway=gene_pathway,
               version="0.0.2",
               maintainer = "zhiganghan <hanzg@zju.edu.cn>", 
               author = "zhiganghan <hanzg@zju.edu.cn>",
               outputDir = ".",
               tax_id=tax_id,
               genus=genus,
               species=species,
               goTable="go")
my_orgdb <- str_c("org.", 
                  str_to_upper(str_sub(genus, 1, 1)) , 
                  species, 
                  ".eg.db", 
                  sep = "")
if (requireNamespace(my_orgdb, quietly = TRUE))
  remove.packages(my_orgdb)
install.packages(my_orgdb, repos = NULL)

#导入org.Hvulgare.eg.db,准备GO分析
library(org.Hvulgare.eg.db)
library(readr)
library(clusterProfiler)
library(DOSE)
library(purrr)
library(tidyverse)
library(clusterProfiler)
#查看包里的基本信息
columns(org.Hvulgare.eg.db)
keys(org.Hvulgare.eg.db)
select(org.Hvulgare.eg.db, keys = "HORVU1Hr1G000620",columns = c("GO","Pathway"))

#GO分析

# 导入需要进行富集分析的基因列表，并转换为向量
DEG <- read.table("CdvsCk", 
                        header = T,
                        sep = "\t",
                        check.names = F,
                        stringsAsFactors = F,
                        row.names = NULL)


DEG_list <- DEG %>% 
  dplyr::filter(abs(log2FoldChange) >= 1 & padj <= 0.05)

gene_list <- as.character(DEG_list$row.names)

#GO 富集,基于基因数目
ego <- enrichGO(gene          = gene_list, #差异基因 vector
                keyType       = "GID",  #差异基因的 ID 类型，需要是 OrgDb 支持的
                OrgDb         = org.Hvulgare.eg.db, #对应的OrgDb
                ont           = "BP", #GO 分类名称，CC BP MF 
                pvalueCutoff  = 1, #Pvalue 阈值
                qvalueCutoff  = 1, #qvalue 阈值
                pAdjustMethod = "BH", #Pvalue 矫正方法
                readable      = FALSE) #TRUE 则展示SYMBOL，FALSE 则展示原来的ID

#将 ego 对象转换为dataframe，新版本可以用as.data.frame(ego)
ego_results<-as.data.frame(ego)
write.table(ego_results, file = "ego_results.txt",
            sep = "\t",
            quote = F)

pdf(file = "ego_barplot.pdf")
barplot(ego, showCategory=20, x = "GeneRatio")
dev.off()
dotplot(ego)
emapplot(ego)
plotGOgraph(ego)
#KEGG分析

pathway2gene <- AnnotationDbi::select(org.Hvulgare.eg.db, 
                                      keys = keys(org.Hvulgare.eg.db), 
                                      columns = c("Pathway","Ko")) %>%
  na.omit() %>%
  dplyr::select(Pathway, GID)

# 导入 Pathway 与名称对应关系

load("kegg_info.RData")

#KEGG pathway 富集
ekp <- enricher(gene_list, 
                TERM2GENE = pathway2gene, 
                TERM2NAME = pathway2name, 
                pvalueCutoff = 1, 
                qvalueCutoff = 1,
                pAdjustMethod = "BH",
                minGSSize = 1)

ekp_results <- as.data.frame(ekp)

barplot(ekp, showCategory=20, x = "GeneRatio")

dotplot(ekp)

emapplot(ekp)

