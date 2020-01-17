# spotify-analysis

Music contains so much information. We can not only compare all the hit songs to conclude the popular music trend, but also analyse the song behaviour of a particular person and get to know more about him/her through just a small music application.

Spotify is one of the top music streaming service around . With 35+ million songs and 170+ million monthly active users, musicians can spread their music and reach the audience easily. Also, it is an ideal platform for users to find their favorite music quickly and conveniently due to its large catalog, collaborative playlists, podcasts, and other attractive features. On the app, music can be browsed or searched via various parameters — such as artists, album, genre, playlist, or record label. Users can create, edit, and share playlists, share tracks on social media, and make playlists with other users.

Recently, I discovered that the Spotify API provides audio features for each song in its database, with which we are able to quantify a song. Therefore, I would like to conduct some music analysis based on it.

This project is divided into two parts. The goal in Part I is to analyze what song characteristics would affect its popularity. To achieve this, I built up a regression model and set the popularity as the dependent variable. The goal in Part II is to create a like song prediction system for a specific spotify user.

For the study, I will access the Spotify Web API, which provides data from the Spotify music catalog. This can be accessed via standard HTTPS requests to an API endpoint.
