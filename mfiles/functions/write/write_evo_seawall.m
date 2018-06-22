function write_evo_seawall(seawall,name)

filename=[name,'_seawall.csv'];
disp(['writing ',filename])

fid = fopen(filename,'w');
FORMATs='%s';
FORMATf='%07.1f';
fprintf(fid,[FORMATs '\n'],'CHAINAGE,WALL_X,WALL_Z');
for i=1:length(seawall.chainage)
    fprintf(fid,FORMATf,seawall.chainage(i));
    fprintf(fid,FORMATs,' ,');
    fprintf(fid,FORMATf,seawall.wall_x(i));%
    fprintf(fid,FORMATs,' ,');
    fprintf(fid,[FORMATf '\n'],seawall.wall_z(i));
end
fclose(fid);
end
