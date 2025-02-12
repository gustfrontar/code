function  [ AccumulatedData ] = AccumulateDataFun( Data , AccumulationLength , AccumulationType );

%Esta funcion acumula los datos, de los pronosticos convirtiendo los pronosticos 10 minutales en horarios o diarios, etc.

NLead=size(Data,2);
NFor=size(Data,1);

EndCol=0;
index=1;
while ( EndCol < NLead )
  IniCol=EndCol+1;
  EndCol=IniCol+AccumulationLength-1;

  if( EndCol > NLead )EndCol=Nlead;end

  if( AccumulationType == 'Mean' )
      AccumulatedData(:,index)=nanmean( Data(:,IniCol:EndCol), 2);
  elseif( AccumulatedData == 'Sum' )
      AccumulatedData(:,index)=nansum( Data(:,IniCol:EndCol), 2);
  end

  index=index+1;
end






