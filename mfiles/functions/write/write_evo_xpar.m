function write_evo_xpar(xpar,name)

filename=[name,'_xpar.csv'];
disp(['writing ',filename])

fid = fopen(filename,'w');
FORMATs='%s';
FORMATf1='%08.2f';
FORMATf2='%010.8f';
fprintf(fid,[FORMATs '\n'],'CHAINAGE,DUNE_ELEVATION,DUNE_SLOPE,A,TRANSITION_SLOPE,OFFSHORE_SLOPE,OFFSHORE_ELEVATION,KE,KA,HBMIN,DESCRIPTION');
for i=1:length(xpar.chainage)
    fprintf(fid,FORMATf1,xpar.chainage(i));% chainage
    fprintf(fid,FORMATs,' ,');
    fprintf(fid,FORMATf1,xpar.z_dune(i));% dune_elevation
    fprintf(fid,FORMATs,' ,');
    fprintf(fid,FORMATf1,xpar.s_dune(i));% dune_slope
    fprintf(fid,FORMATs,' ,');
    fprintf(fid,FORMATf2,xpar.a(i));% a
    fprintf(fid,FORMATs,' ,');
    fprintf(fid,FORMATf2,xpar.s_trans(i)); % transition slope
    fprintf(fid,FORMATs,' ,');
    fprintf(fid,FORMATf2,xpar.s_off(i));% offshore_slope
    fprintf(fid,FORMATs,' ,');
    fprintf(fid,FORMATf1,xpar.z_off(i)); % offshore elevation
    fprintf(fid,FORMATs,' ,');
    fprintf(fid,FORMATf2,xpar.ke); % KE
    fprintf(fid,FORMATs,' ,');
    fprintf(fid,FORMATf2,xpar.ka); % KA
    fprintf(fid,FORMATs,' ,');
    fprintf(fid,FORMATf1,xpar.hbmin); % HB min
    fprintf(fid,FORMATs,' ,');
    fprintf(fid,[FORMATs '\n'],'middle'); % Description
end
fclose(fid);
end
