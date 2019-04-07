require(httr)
require(dplyr)
require(rvest)
require(mongolite)

# 目標網站
url <- 'https://www.ettoday.net/show_roll.php'
# 建立一個空list
urlList <- c()

# 開始爬取前10roll的網址
# 用for迴圈
for(i in 1:10){
doc <- url %>% POST(.,body = list('offset'=i,
                           'tPage'=3,
                           'tFile'='20190326.xml',
                           'tOt'=0,
                           'tSi'=100,
                           'tAr'=0)) %>% content()

newUrlList <- doc %>% html_nodes('a') %>% html_attr('href')
urlList <- urlList %>% append(.,newUrlList)
}
# 用apply
urlList <- lapply(1:10, function(i)
{
  doc <- url %>% POST(.,body = list('offset'=i,
                                    'tPage'=3,
                                    'tFile'='20190326.xml',
                                    'tOt'=0,
                                    'tSi'=100,
                                    'tAr'=0)) %>% content()
  
  newUrlList <- doc %>% html_nodes('a') %>% html_attr('href')
  return(newUrlList)
}) %>% unlist 

# 確定是否拿到100個網址
urlList %>% unique() %>% length()

# 資料庫connection
conn <- mongo(db = db,collection = collection,url = url)

# 開始爬取100篇新聞的內文, URLencode確保編碼正確
# 把抓到的post, title, content存成dataframe
# trycatch 
# sleep
urlList %>% lapply(.,function(url) {
  res <- tryCatch({
    url <- url %>% paste0('https://www.ettoday.net',.) %>% URLencode(.)
    Sys.sleep(runif(1,0.5,2.5))
    doc <- url %>% GET(.) %>% content(.)
    post <- doc %>% html_node('time') %>% html_attr('datetime')
    title <- doc %>% html_node('h1') %>% html_text()
    content <- doc %>% html_nodes('.story p') %>% html_text() %>% paste(.,collapse = '')
    articleInfo <- data.frame('post'=post,'title'=title,'content'=content, stringsAsFactors = T)
    conn$insert(articleInfo,stop_on_error = F)
    1
  },error = function(e) {
    0
  })
  return(res)
})












