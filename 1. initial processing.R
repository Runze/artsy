setwd('/Users/Runze/Github/artsy')

#load followers' profiles (list parsed from json)
load(file = 'follower_profile.RData')

#function to only extract relevant info
extract_info = function(elem) {
  return (elem[c('id', 'name', 'description', 'followers_count', 'friends_count', 'location', 'created_at')])
}

#subset data and transform into data.frame
follower_profile_sub = lapply(follower_profile, extract_info)
follower_profile_sub = lapply(follower_profile_sub, lapply, function(x) ifelse(is.null(x), '', x))
followers = data.frame(matrix(unlist(follower_profile_sub), nrow = length(follower_profile), byrow = T))
names(followers) = names(follower_profile_sub[[1]])
followers$location = as.character(followers$location)

save(followers, file = 'followers.RData')