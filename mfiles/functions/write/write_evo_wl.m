function write_evo_wl(wl,name)

filename=[name,'_wl.csv'];
disp(['writing ',filename])

fid = fopen(filename,'w');
FORMATs='%s';
FORMATf='%05.2f';
fprintf(fid,[FORMATs '\n'],'TIME,WL');
for i=1:length(wl.time);
    fprintf(fid,FORMATs,datestr(wl.time(i),24));%
    fprintf(fid,FORMATs,' ');%
    fprintf(fid,FORMATs,datestr(wl.time(i),15));%
    fprintf(fid,FORMATs,',');
    fprintf(fid,[FORMATf '\n'],wl.WL(i));%
end
fclose(fid);
end
