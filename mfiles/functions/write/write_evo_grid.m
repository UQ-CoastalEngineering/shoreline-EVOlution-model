function write_evo_grid(grid,name)

filename=[name,'_grid.csv'];
disp(['writing ',filename])
fid = fopen(filename,'w');
FORMATs='%s';
FORMATf='%010.1f';
fprintf(fid,[FORMATs '\n'],'CHAINAGE,BASE_X,BASE_Y,END_X,END_Y');
for i=1:length(grid.chainage)
fprintf(fid,FORMATf,grid.chainage(i));
fprintf(fid,FORMATs,' ,');
fprintf(fid,FORMATf,grid.base_x(i));
fprintf(fid,FORMATs,' ,');
fprintf(fid,FORMATf,grid.base_y(i));
fprintf(fid,FORMATs,' ,');
fprintf(fid,FORMATf,grid.end_x(i));
fprintf(fid,FORMATs,' ,');
fprintf(fid,[FORMATf '\n'],grid.end_y(i));
end
fclose(fid);
end
