# book-tracker
a app for tracking when books you want to get come into stock at the local used bookstore! 


I would like to write and deploy a full scale app in Clojure that teaches me libraries through experience! In particular, it should:
1) make at least one HTTP request
2) use a database 
3) use a schema library like malli to verify inputs/outputs when connecting to 1) / 2). 
4) have a UI! 
5) be (reasonably) easy to deploy
6) has a full test suite

My current idea is to build a simple CRUD webapp that allows people to get notified when a book comes into stock at a local used bookshop. They list all their inventory online + keep it regularly updated, so the app would allow someone to: 

1) enter a list of books they want to get notified for. If the book is already there, just let them know before setting up the notification, and allow them to opt out and go pick it up. Otherwise, they should get an email when the book comes into stock. 

2) manage the list of notifications they have configured (list them, update the email attached to a notification for a particular book, update the email for all books, delete notifications configurations). 

3) share a list of pre-baked book notifications with a friend, with a hole for that friend's email
