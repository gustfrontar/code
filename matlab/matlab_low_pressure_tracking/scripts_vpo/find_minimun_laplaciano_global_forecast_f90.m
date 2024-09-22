function find_minimun_laplaciano_global_forecast_f90(DataFile,LatFile,LonFile,ClimFile,FortranProgramPath,OutputFileName)

%This function is a matlab wrapper for the fortran program find_minimuns.
%This function just links the files that the program needs.

%LINK THE FILES.
    unix(['ln -fs ' DataFile  ' ' FortranProgramPath '/datainput.bin']);
    unix(['ln -fs ' LatFile   ' ' FortranProgramPath '/latinput.bin']);
    unix(['ln -fs ' LonFile   ' ' FortranProgramPath '/loninput.bin']);
    unix(['ln -fs ' ClimFile  ' ' FortranProgramPath '/climinput.bin']);
    
%RUN THE PROGRAM

    [nada test]=unix([FortranProgramPath '/find_minimun_laplaciano_forecast']);
    if(test~=0)
        fprintf('WARNING!! THE PROGRAM FIND_MINIMUN RETURNS AN ERROR STATUS');
    end
     
    
%GET THE OUTPUT PATH

    unix(['mv  ' FortranProgramPath '/minimun_out.bin ' OutputFileName ]);
    


end
    
 