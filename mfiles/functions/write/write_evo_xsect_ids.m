function write_evo_xsect_ids(xsect_ids,name)

filename=[name,'_xsect_ids.csv'];
disp(['writing ',filename])

fid = fopen(filename,'w');
FORMATs='%s';
FORMATf='%05.2f';
fprintf(fid,[FORMATs '\n'],'CHAINAGE,ID');
for i=1:length(xsect_ids.chainage)
fprintf(fid,FORMATf,xsect_ids.chainage(i));
fprintf(fid,FORMATs,',');
fprintf(fid,[FORMATf '\n'],xsect_ids.name(i));% 
end
fclose(fid);
end
