library(dplyr)
library(twitteR)
library(jsonlite)
library(igraph)
library(visNetwork)

# Idea is to download all #ISME16 or #isme16 tweets and figure out some neat stats
# about them, like who sent them, who's been mentioned, do some pagerank, some
# graphing, etc.

# Set up Twitter access
twitter_credentials <- readChar("~/twitter_credentials.json", 1000) %>% fromJSON
setup_twitter_oauth(twitter_credentials$consumer_key,
                    twitter_credentials$consumer_secret,
                    twitter_credentials$access_token,
                    twitter_credentials$access_secret)

# Get tweets!
isme16_tweets <- searchTwitter("#ISME16", n = 10000) %>% twListToDF() %>% tbl_df

isme16_tweets %>% filter(created > "2016-08-21") %>%
  ggplot(aes(created)) + geom_histogram()




tweeter_sum <- isme16_tweets %>% group_by(screenName) %>%
  summarize(numTweetsPosted = n(), numRetweetsPosted = sum(isRetweet),
            numTimesRetweeted = sum(retweetCount), numTimesFavorited = sum(favoriteCount))
replies <- isme16_tweets %>% filter(!is.na(replyToSN))
rts <- isme16_tweets %>% filter(isRetweet) %>%
  mutate(originalSN = gsub("RT @([^:]*):.*", "\\1", text))

# Hm need to think about what to do here...

# Build up an igraph
isme16_igraph <- make_empty_graph() +
  vertices(tweeter_sum$screenName, numTweetsPosted = tweeter_sum$numTweetsPosted,
           numRetweetsPosted = tweeter_sum$numRetweetsPosted,
           numTimesRetweeted = tweeter_sum$numTimesRetweeted,
           numTimesFavorited = tweeter_sum$numTimesFavorited) +
  vertices(filter(replies, !(replyToSN %in% tweeter_sum$screenName))$replyToSN,
           numTweetsPosted = 0, numRetweetsPosted = 0,
           numTimesRetweeted = 0, numTimesFavorited = 0) +
  vertices(filter(rts, !(originalSN %in% tweeter_sum$screenName),
                  !(originalSN %in% replies$replyToSN))$originalSN,
           numTweetsPosted = 0, numRetweetsPosted = 0,
           numTimesRetweeted = 0, numTimesFavorited = 0) +
  edges(as.vector(rbind(replies$screenName, replies$replyToSN)),
        favoriteCount = replies$favoriteCount,
        retweetCount = replies$retweetCount,
        linkType = "reply") +
  edges(as.vector(rbind(rts$screenName, rts$originalSN)),
        favoriteCount = rts$favoriteCount,
        retweetCount = rts$retweetCount,
        linkType = "retweet")

pr <- page.rank(isme16_igraph,
                weights = E(isme16_igraph)$favoriteCount +
                  E(isme16_igraph)$retweetCount)$vector

isme16_igraph <- set_vertex_attr(isme16_igraph, "pagerank", names(pr), pr)

isme16_pr <- subgraph(isme16_igraph, which(V(isme16_igraph)$pagerank >= 0.004))

isme16_vn <- toVisNetworkData(isme16_pr)
isme16_nodes <- isme16_vn$nodes %>% tbl_df %>%
  rename(value = pagerank)
isme16_edges <- isme16_vn$edges %>% tbl_df %>%
  mutate(color = ifelse(linkType == "retweet", "blue", "red")) %>%
  rename(value = retweetCount)

visNetwork(nodes = isme16_nodes, edges = isme16_edges) %>%
  visIgraphLayout() %>%
  visOptions(highlightNearest = TRUE)

