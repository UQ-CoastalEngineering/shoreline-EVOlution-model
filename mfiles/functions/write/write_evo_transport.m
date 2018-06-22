function write_evo_transport(transport,name)

filename=[name,'_transport.csv'];
disp(['writing ',filename])

fid = fopen(filename,'w');
FORMATs='%s';
FORMATf='%07.1f';
fprintf(fid,[FORMATs '\n'],'TIME,TRANSPORT');
for i=1:length(transport.time)
	fprintf(fid,FORMATs,datestr(transport.time(i),24));% 
	fprintf(fid,FORMATs,' ');% 
	fprintf(fid,FORMATs,datestr(transport.time(i),15));% 
	fprintf(fid,FORMATs,',');
    	fprintf(fid,[FORMATf  '\n'],transport.Q(i));%
end
fclose(fid);
end
