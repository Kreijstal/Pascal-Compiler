{ Minimal FGL (Free Pascal Generic Library) unit stub
  
  NOTE: Full generic/template support is not yet implemented in this compiler.
  
  This stub allows FGL-using code to compile by providing type declarations
  that the parser recognizes. Actual specialized implementations must be
  provided in the using program or specialized manually.
  
  The test record_operators_fgl.p provides its own TMyRecordList implementation
  inline, so this unit just needs to exist for the 'uses FGL' clause.
}

unit FGL;

interface

{ The generic TFPGList declaration is recognized by the parser but not
  yet fully supported by the type system. Users must provide specialized
  versions in their own code until full generic support is implemented. }

implementation

end.
