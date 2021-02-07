def to_tuple:
  to_entries | map([(.key | tonumber), .value]);

to_entries
  | map(.value
       | { name: .name
         , abbrev: .abbrev
         , chapters: (.chapters | to_entries | map([(.key | tonumber), (.value | to_tuple)]))
         })
