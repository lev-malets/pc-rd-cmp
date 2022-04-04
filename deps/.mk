
$(clone angstrom,'https://github.com/inhabitedtype/angstrom',b0e7849ec59746b44753a4c5f5954ba95d1c6c5a)
$(patch angstrom,$D/angstrom.patch)

$(clone syntax,'https://github.com/rescript-lang/syntax',d9c44bb22892dc4e77334f064d8bccd119fecde9)
$(patch syntax,$D/syntax.patch)

$K/done: $K/patch/angstrom $K/patch/syntax
