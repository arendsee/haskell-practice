{-# LANGUAGE TemplateHaskell #-}

import Control.Lens hiding (element)

data Genome
  = Genome {
    _species  :: String
  , _scaffold :: [Scaffold]
  } deriving(Show)

data Scaffold
  = Scaffold {
      _scafid :: String
    , _gene   :: [Gene]
  } deriving(Show)

data Gene
  = Gene {
    _geneid :: String
  , _models :: [GeneModel]
  } deriving(Show)

data GeneModel
  = GeneModel {
    _modelid :: String
  , _strand  :: Strand
  , _orf     :: [Interval]
  , _rna     :: [Interval]
  } deriving(Show)

data Strand = Plus | Minus | NoSt deriving(Show)

data Interval
  = Interval {
    _start :: Int
  , _stop  :: Int
  } deriving(Show)

data Entry
  = Entry {
    _entry_id     :: String
  , _entry_type   :: String
  , _entry_start  :: Int
  , _entry_stop   :: Int
  , _entry_strand :: Strand
  , _entry_parent :: String
  } deriving(Show)

data GFF
  = GFF {
    _gff_species :: String
  , _gff_entries :: [Entry]
  } deriving(Show)

gff = GFF "unicorn" [
    Entry "scaf1"  "scaf" 1    9999 NoSt ""

  , Entry "gene1"  "gene" 1    1000 Plus "scaf1"

  , Entry "mrna1"  "mRNA" 1    1000 Plus "gene1"
  , Entry "exon1"  "exon" 1    200  Plus "mrna1"
  , Entry "exon2"  "exon" 300  700  Plus "mrna1"
  , Entry "code1"  "code" 1    200  Plus "mrna1"
  , Entry "code2"  "code" 300  700  Plus "mrna1"

  , Entry "mrna2"  "mRNA" 1    1000 Plus "gene1"
  , Entry "exon3"  "exon" 1    200  Plus "mrna2"
  , Entry "exon4"  "exon" 300  700  Plus "mrna2"
  , Entry "code3"  "code" 1    200  Plus "mrna2"
  , Entry "code4"  "code" 700  300  Plus "mrna2" -- illegal coordinates
  ]

makeLenses ''Genome
makeLenses ''Scaffold
makeLenses ''Gene
makeLenses ''GeneModel
makeLenses ''Interval
makeLenses ''Entry
makeLenses ''GFF

incrementStart :: GFF -> GFF
incrementStart = over (gff_entries . traverse . entry_start) (+ 1)
