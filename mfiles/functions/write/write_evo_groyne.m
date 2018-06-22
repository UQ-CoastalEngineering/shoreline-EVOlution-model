function write_evo_groyne(groyne,name)

filename=[name,'_groynes.csv'];
disp(['writing ',filename])

fid = fopen(filename,'w');
FORMATs='%s';
FORMATf='%07.1f';
fprintf(fid,[FORMATs '\n'],'CHAINAGE,GROYNE_X,START_TIME,END_TIME,COMMENT');
for i=1:length(groyne.chainage)
    fprintf(fid,FORMATf,groyne.chainage(i));
    fprintf(fid,FORMATs,' ,');
    fprintf(fid,FORMATf,groyne.width(i));
    fprintf(fid,FORMATs,' ,');
    fprintf(fid,FORMATs,datestr(groyne.time(1),24));
    fprintf(fid,FORMATs,' ');
    fprintf(fid,FORMATs,datestr(groyne.time(1),15));
    fprintf(fid,FORMATs,' ,');
    fprintf(fid,FORMATs,datestr(groyne.time(end),24));
    fprintf(fid,FORMATs,' ');
    fprintf(fid,FORMATs,datestr(groyne.time(end),15));
    fprintf(fid,FORMATs,' ,');
    fprintf(fid,[FORMATs '\n'],'HEADLAND');
end
fclose(fid);
end
