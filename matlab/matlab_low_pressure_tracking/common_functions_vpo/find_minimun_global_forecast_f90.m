function find_minimun_global_forecast_f90(DataFile,LatFile,LonFile,ClimFile,FortranProgramPath,OutputFileName,tmpwork)

%This function is a matlab wrapper for the fortran program find_minimuns.
%This function just links the files that the program needs.

cd(tmpwork);

%LINK THE FILES.
    unix(['ln -fs ' DataFile  ' ' tmpwork '/datainput.bin']);
    unix(['ln -fs ' LatFile   ' ' tmpwork '/latinput.bin']);
    unix(['ln -fs ' LonFile   ' ' tmpwork '/loninput.bin']);
    unix(['ln -fs ' ClimFile  ' ' tmpwork '/climinput.bin']);
    unix(['ln -fs ' FortranProgramPath '/find_minimun_forecast ./find_minimun_forecast'])
    
%RUN THE PROGRAM

    [nada test]=unix([tmpwork '/find_minimun_forecast']);
    if(test~=0)
        fprintf('WARNING!! THE PROGRAM FIND_MINIMUN RETURNS AN ERROR STATUS');
    end
     
    
%GET THE OUTPUT PATH

    unix(['mv  ' FortranProgramPath '/minimun_out.bin ' OutputFileName ]);
    


end
    
 