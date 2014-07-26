setwd('/Users/Runze/Github/artsy')

#load followers' profiles (list parsed from json)
load(file = 'data/follower_profile.RData')

#function to only extract relevant info
extract_info = function(elem) {
  return (elem[c('id', 'name', 'description', 'followers_count', 'friends_count', 'location', 'created_at')])
}

#subset data and transform into data.frame
follower_profile_sub = lapply(follower_profile, extract_info)

#replace null items to blank (because not all profiles share the same amount of information)
follower_profile_sub = lapply(follower_profile_sub, lapply, function(x) ifelse(is.null(x), '', x))

followers = data.frame(matrix(unlist(follower_profile_sub), nrow = length(follower_profile), byrow = T))
names(followers) = names(follower_profile_sub[[1]])
followers$location = as.character(followers$location)

save(followers, file = 'data/followers.RData')
