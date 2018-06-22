function write_evo_xsect_ids_eq(xsect_ids_eq,name)

filename=[name,'_xsect_ids_eq.csv'];
disp(['writing ',filename])

fid = fopen(filename,'w');
FORMATs='%s';
FORMATf='%05.2f';
fprintf(fid,[FORMATs '\n'],'CHAINAGE,ID');
for i=1:length(xsect_ids_eq.chainage)
fprintf(fid,FORMATf,xsect_ids_eq.chainage(i));
fprintf(fid,FORMATs,',');
fprintf(fid,[FORMATf '\n'],xsect_ids_eq.name(i));% 
end
fclose(fid);
end
