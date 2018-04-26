resource MiniResEpo = {
  param PNumber = Sg | Pl ;
	PCase = Nom | Acc ;
	PPolarity = Pos | Neg ;
  oper
    Determiner : Type = { s : PCase => Str ; n : PNumber } ;
    Noun : Type = { s : PNumber => PCase => Str } ;
    ProperName : Type = { s : PCase => Str } ;
    NounPhrase : Type = { s : PCase => Str ; n : PNumber } ;
    Verb, TransVerb : Type = { s : Str } ;
    VerbPhrase : Type = { s : Str ; o : PNumber => Str } ;
    Adjective : Type = { s : PNumber => PCase => Str } ;
    AdjectivePhrase : Type = { s : PNumber => PCase => Str } ;
    Adverb : Type = { s : Str } ;
    Conjunction : Type = { s : Str } ;
    Preposition : Type = { s : Str } ;
    Pronoun : Type = { s : PCase => Str ; n : PNumber } ;
    Polarity : Type = { s : Str ; p : PPolarity } ;
    Clause : Type = { s : PPolarity => Str } ;
    
    mkDet = overload {
      mkDet : Str -> PNumber -> Determiner = \s,n ->
	{ s = \\_ => s ; n = n } ;
      mkDet : Str -> Str -> PNumber -> Determiner = \nom,acc,n ->
	{ s = table { Nom => nom ; Acc => acc } ; n = n } ;
      } ;
    
    mkN : Str -> Noun = \s ->
      {
	s = inflectN s
      };

    mkPN : Str -> ProperName = \s ->
      {
	s = table {
	  Nom => s ;
	  Acc => s + "n"
	  }
      } ;

    mkV : Str -> Verb = \s ->
      {
	s = case s of {
	  far + "i" => far + "as" ;
	  _ => s
	  }
      };
    
    mkV2 : Str -> TransVerb = \s -> mkV s ;

    mkA : Str -> Adjective = \s ->
      {
	s = inflectN s
      } ;
      
    mkAdv = sid ;

    mkConj = sid ;

    mkPrep = sid ;

    mkPron : Str -> PNumber -> Pronoun = \s,n ->
      {
	s = table { Nom => s ;
		    Acc => s + "n"
	  } ;
	n = n
      } ;
    
    sid : Str -> { s : Str }  = \s -> { s = s } ;
    inflectN : Str -> PNumber => PCase => Str = \s ->
      table {
	Sg => table {
	  Nom => s ;
	  Acc => s + "n"
	  } ;
	Pl => table {
	  Nom => s + "j" ;
	  Acc => s + "jn"
	  }
      } ;
}