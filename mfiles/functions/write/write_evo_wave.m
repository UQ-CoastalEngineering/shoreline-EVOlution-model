function write_evo_wave(wave,name)

filename=[name,'_wave.csv'];
disp(['writing ',filename])
 
fid = fopen(filename,'w');
FORMATs='%s';
FORMATf='%05.2f';
fprintf(fid,[FORMATs '\n'],'TIME,WVHT,WVPER,WVDIR');
for i=1:length(wave.time);
fprintf(fid,FORMATs,datestr(wave.time(i),24));% 
fprintf(fid,FORMATs,' ');% 
fprintf(fid,FORMATs,datestr(wave.time(i),15));% 
fprintf(fid,FORMATs,',');
fprintf(fid,FORMATf,wave.hsig(i));% 
fprintf(fid,FORMATs,',');
fprintf(fid,FORMATf,wave.tp(i));% 
fprintf(fid,FORMATs,',');
fprintf(fid,[FORMATf '\n'],wave.wdir(i));% 
end
fclose(fid);
end
