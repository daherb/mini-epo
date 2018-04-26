concrete MiniGrammarEpo of MiniGrammar = open MiniResEpo in {
  lincat
    Det = Determiner ;
    N = Noun ;
    CN = Noun ;
    PN = ProperName ;
    NP = NounPhrase ;
    Pron = NounPhrase ;
    V = Verb ;
    V2 = TransVerb ;
    VP = VerbPhrase ;
    A = Adjective ;
    AP = AdjectivePhrase ; 
    Adv = Adverb ;
    Conj = Conjunction ;
    Prep = Preposition ;
    Pol = Polarity ;
    Cl = Clause ; 
    S, Utt = Str ;
  lin
    -- Phrase
    --     UttS      : S  -> Utt ;
    UttS s = s ;
    --     UttNP     : NP -> Utt ;
    UttNP np = np.s ! Nom ;

-- -- Sentence
    --     UsePresCl : Pol -> Cl  -> S ;       -- John does not walk ---s
    UsePresCl pol cl = pol.s ++ cl.s ! pol.p ;
    
    --     PredVP    : NP -> VP -> Cl ;        -- John walks / John does not walk
    PredVP np vp =
      {
	s = table {
	  Pos => np.s ! Nom ++ vp.s ;
	  Neg => np.s ! Nom ++ "ne" ++ vp.s
	  }
      } ;
-- -- Verb
    --     UseV      : V   -> VP ;             -- sleep
    UseV v = v ** { o = \\_ => ""} ;
    --     ComplV2   : V2  -> NP -> VP ;       -- love it  ---s
    ComplV2 v2 np = v2 ** { o = \\_ => np.s ! Acc } ;
    --     UseAP     : AP  -> VP ;             -- be small ---s
    UseAP ap = { s = "estas" ; o = \\n => ap.s ! n ! Nom } ;
    --     AdvVP     : VP -> Adv -> VP ;       -- sleep here
    AdvVP vp adv =
      vp ** { o = \\c => vp.o ! c ++ adv.s } ; 

-- -- Noun
    --     DetCN     : Det -> CN -> NP ;       -- the man
    DetCN det cn = { s = \\c => det.s ! c ++ cn.s ! det.n ! c ; n = det.n } ;
    --     UsePN     : PN -> NP ;              -- John
    UsePN pn = pn ** { n = Sg } ;
    --     UsePron   : Pron -> NP ;            -- he
    UsePron p = p ;
    --     MassNP    : CN -> NP ;              -- milk
    MassNP m = { s = m.s ! Sg ; n = Sg } ;
    --     a_Det     : Det ;                   -- indefinite singular ---s
    a_Det = mkDet "" Sg ;
    --     aPl_Det   : Det ;                   -- indefinite plural   ---s
    aPl_Det = mkDet "" Pl ;
    --     the_Det   : Det ;                   -- definite singular   ---s
    the_Det = mkDet "la" Sg ;
    --     thePl_Det : Det ;                   -- definite plural     ---s
    thePl_Det = mkDet "la" Pl ;
    --     UseN      : N -> CN ;               -- house
    UseN n = n ;
    --     AdjCN     : AP -> CN  -> CN ;       -- big house
    AdjCN adj cn =
      { s = \\n,c => adj.s ! n ! c ++ cn.s ! n ! c } ;
-- -- Adjective
    --     PositA    : A  -> AP ;              -- warm
    PositA a = a ;
-- -- Adverb
    --     PrepNP    : Prep -> NP -> Adv ;     -- in the house
    PrepNP prep np = { s = prep.s ++ np.s ! Nom } ;

-- -- Conjunction
    --     CoordS    : Conj -> S -> S -> S ;   -- he walks and she runs ---s
    CoordS conj s1 s2 =
      s1 ++ conj.s ++ s2 ;

-- -- Tense
    --     PPos      : Pol ;                   -- I sleep  [positive polarity]
    PPos = { s = "" ; p = Pos } ;
    --     PNeg      : Pol ;                   -- I do not sleep [negative polarity]
    PNeg = { s = "" ; p = Neg } ;

-- -- Structural
    --     and_Conj  : Conj ;
    and_Conj = mkConj "kaj" ;
    --     or_Conj   : Conj ;
    or_Conj = mkConj "aŭ" ;
    
    --     every_Det : Det ;
    every_Det = mkDet "ĉiu" "ĉiun" Pl ;
    
    --     in_Prep   : Prep ;
    in_Prep = mkPrep "en" ;
    --     on_Prep   : Prep ;
    on_Prep = mkPrep "sur" ;
    --     with_Prep : Prep ;
    with_Prep = mkPrep "kun" ;

    --     i_Pron     : Pron ;
    i_Pron = mkPron "mi" Sg ;
    
    --     youSg_Pron : Pron ;
    youSg_Pron = mkPron "vi" Sg ;
    
    --     he_Pron    : Pron ;
    he_Pron = mkPron "li" Sg ; 

    --     she_Pron   : Pron ;
    she_Pron = mkPron "ŝi" Sg ;
    
    --     we_Pron    : Pron ;
    we_Pron = mkPron "ni" Pl ;
    
    --     youPl_Pron : Pron ;
    youPl_Pron = mkPron "vi" Pl ;
    
    --     they_Pron  : Pron ;
    they_Pron = mkPron "ili" Pl ;
}