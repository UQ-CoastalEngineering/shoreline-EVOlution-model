function write_evo_icond(icond,name)

filename=[name,'_icond.csv'];
disp(['writing ',filename])

fid = fopen(filename,'w');
FORMATs='%s';
FORMATf='%07.1f';
fprintf(fid,[FORMATs '\n'],'CHAINAGE,X0,Z0,X1,Z1,dx');
for i=1:length(icond.x0)
fprintf(fid,FORMATf,icond.chainage(i));
fprintf(fid,FORMATs,' ,');
fprintf(fid,FORMATf,icond.x0(i));
fprintf(fid,FORMATs,' ,');
fprintf(fid,FORMATf,icond.z0(i));
fprintf(fid,FORMATs,' ,');
fprintf(fid,FORMATf,icond.x1(i));
fprintf(fid,FORMATs,' ,');
fprintf(fid,[FORMATf],icond.z1(i));
fprintf(fid,FORMATs,' ,');
fprintf(fid,[FORMATf '\n'],(icond.x1(i)-icond.x0(i))/1);
end
fclose(fid);
end
