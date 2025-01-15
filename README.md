# BLW's KEG website

Using (elm-pages)[https://elm-pages.com/] to make KEG in the web format as simple, indexable, and beautiful as possible.

Of course, redability is a must.

(elm-ui)[https://github.com/mdgriffith/elm-ui] was used, instead of CSS.

## Setup Instructions

- Clone project;
- Clone a KEG (will have to add front-matter for all notes);
- Move KEG to `/content/blog`;
- `elm-pages dev`;
- Access `localhost:1234/blog`;

``` sh
git clone https://github.com/BuddhiLW/elm-keg.git
cd elm-keg
mkdir -p ./content
git clone https://github.com/BuddhiLW/blw-zet.git ./content/blog
```

``` sh
elm-pages dev
```


