# TodoMVC in Elm - [Try It!](http://robertjlooby.github.io/elm-todomvc)

## Project Structure

All of the Elm code lives in `Todo.elm` and relies on the [elm-html][] library. 

[elm-html]: http://package.elm-lang.org/packages/evancz/elm-html/latest 

There also is a port handler set up in `index.html` to set the focus on
particular text fields when necessary.

## Build Instructions

Run the following command from the root of this project:

```bash
elm-make Todo.elm --output elm.js
```

Then open `index.html` in your browser!

This has been modified from the
[original version](https://github.com/evancz/elm-todomvc)
which stored the tasks in local storage to make HTTP requests to any backend
conforming to the [Todo-Backend](http://www.todobackend.com/index.html)
spec. Just the `url` needs to updated to change which backend it is pointing
at.
