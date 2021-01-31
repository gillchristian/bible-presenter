def verses:
  reduce .[] as $verse ({}; .[$verse.key + 1 | tostring] = $verse.value);

def chapters:
  reduce .[] as $chapter
    ( {};
       .[$chapter.key + 1 | tostring] = ( $chapter.value | to_entries | verses )
    );

reduce .[] as $book
  ( {};
    .[$book.abbrev] =
        { name: $book.name
        , abbrev: $book.abbrev
        , chapters: $book.chapters | to_entries | chapters
        }
  )
 
