FOR /L %%A IN (1,1,1000) DO (
  swipl -s qRRL.pl --quiet -t runbatchdefault
)
