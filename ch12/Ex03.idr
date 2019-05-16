-- Section 12.3, exercises 3 and 4

record Votes where
       constructor MkVotes
       upvotes : Integer
       downvotes : Integer

record Article where
       constructor MkArticle
       title : String
       url : String
       score : Votes

initPage : (title : String) -> (url : String) -> Article
initPage title url = MkArticle title url (MkVotes 0 0)

-- ex 3
getScore : Article -> Integer
getScore article = let artScore = score article in
                   upvotes artScore - downvotes artScore

-- ex 4
addUpvote : Article -> Article
addUpvote article = record { score->upvotes $= (+1) } article

addDownvote : Article -> Article
addDownvote article = record { score-> downvotes $= (+1)} article

-- test data
badSite : Article
badSite = MkArticle "Bd page" "http://example.com" (MkVotes 5 47)

goodSite : Article
goodSite = MkArticle "Good page" "http://example.com/good" (MkVotes 101 7)

