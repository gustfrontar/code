

nt=size(TrajStruct,2)

  figure 
  hold on

  for it=1:nt

    if( TrajStruct(it).length > 24 )

    plot(TrajStruct(it).minlon,TrajStruct(it).minlat,'o-')

    end

  end
