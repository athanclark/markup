# markup

> **WARNING**: This project is in it's infancy, please don't expect it to work. 
> Thank you :)

A generic interface to chunks of markup.

In this library, we try to ambiguate _types_ of markup via data types, namely 
`Image` and `JavaScript` as examples. From here, we can issue a call to 
`renderMarkup` on these abstract labels, which will return some markup wrapped 
in a monad - in our case, we've made a few monad readers to represent the 
different ways a single idea can be deployed as markup.

We have three different deployment schemes - inline, hosted, and local. Inline 
markup simply tries to take the information in question and insert it inside the 
markup. Hosted markup simply takes the idea and expects it to be somewhere else, 
possibly hosted in a CDN. Local markup tries to utilize 
[urlpath](https://github.com/athanclark/urlpath) as the means of representing a 
local link (absolute, relative, and grounded methods are in another monad - 
inside the `HtmlT m ()` of lucid, in this case. Urlpath isn't supported for 
Blaze-html.)

## Installation

```bash
cabal install markup
```

## Usage

It's a little awkward at the moment:

```haskell
image' = renderMarkup Image :: Monad m => HostedMarkupT m T.Text (Html ())

image = runHostedMarkupT image' "foo.png"

λ> renderText image

<img src="foo.png">
```

We could also overload the `run*` monad transformer actions of each deployment 
scheme, allowing for the decision to be made just with a type coersion. Maybe in 
v0.0.2 :)

Here is the same example, going relative instead:

```haskell
image' = renderMarkup Image :: (Monad m, Url UrlString AbsoluteUrl) => LocalMarkupT UrlString m (HtmlT AbsoluteUrl ())

λ> (runUrlReader $ renderTextT $ runIdentity $ runLocalMarkupT image' $
     "foo.png" <?> ("key","bar")
   ) "example.com"

"<img src=\"example.com/foo.png?key=bar\">"
```

## How to run tests

```
cabal configure --enable-tests && cabal build && cabal test
```

## Contributing

Fork, PR, repeat.
