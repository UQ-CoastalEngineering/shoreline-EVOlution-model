function write_nc_evo(lookup,nc_file)
disp(['writing ',nc_file])

% Main Attributes
Name{1}='Origin';
Name{2}='Description';
Name{3}='Offshore_Depth';
Name{4}='Offshore_Contour_Rotation';
Value{1}='Netcdf file produced by Uriah Gravois (University of Queensland)';
Value{2}='Wave transformation table';
Value{3}=30;
Value{4}=0;

nccreate(nc_file,'Chainage','Dimensions',{'Ny',lookup.Ny},'Datatype','single','Format','classic');
ncwrite(nc_file,'Chainage',lookup.Chainage{1});
ncwriteatt(nc_file,'Chainage','units','metres');
ncwriteatt(nc_file,'Chainage','longname','Chainage');

nccreate(nc_file,'Depth','Dimensions',{'Ny',lookup.Ny},'Datatype','single','Format','classic');
ncwrite(nc_file,'Depth',lookup.Depth{1});
ncwriteatt(nc_file,'Depth','units','metres');
ncwriteatt(nc_file,'Depth','longname','Intermediate depth');

nccreate(nc_file,'Contour_Rotation','Dimensions',{'Ny',lookup.Ny},'Datatype','single','Format','classic');
ncwrite(nc_file,'Contour_Rotation',lookup.Contour_Rotation{1});
ncwriteatt(nc_file,'Contour_Rotation','units','degrees anticlockwise');
ncwriteatt(nc_file,'Contour_Rotation','longname','Intermediate contour rotation');

nccreate(nc_file,'Height','Dimensions',{'NHt',lookup.NHt},'Datatype','single','Format','classic');
ncwrite(nc_file,'Height',lookup.Height{1});
ncwriteatt(nc_file,'Height','units','degrees anticlockwise');
ncwriteatt(nc_file,'Height','longname','Significant wave height');

nccreate(nc_file,'Period','Dimensions',{'Nper',lookup.Nper},'Datatype','single','Format','classic');
ncwrite(nc_file,'Period',lookup.Period{1});
ncwriteatt(nc_file,'Period','units','seconds');
ncwriteatt(nc_file,'Period','longname','Peak wave period');

nccreate(nc_file,'Direction','Dimensions',{'Ndir',lookup.Ndir},'Datatype','single','Format','classic');
ncwrite(nc_file,'Direction',lookup.Direction{1});
ncwriteatt(nc_file,'Period','units','degrees wave convention');
ncwriteatt(nc_file,'Period','longname','Input Wave Direction');

nccreate(nc_file,'K','Dimensions',{'Ndir',lookup.Ndir,'Nper',lookup.Nper,'NHt',lookup.NHt,'Ny',lookup.Ny},'Datatype','single','Format','classic');
ncwrite(nc_file,'K',lookup.K{1});
ncwriteatt(nc_file,'K','units','-');
ncwriteatt(nc_file,'K','longname','Wave height transformation coefficient');

nccreate(nc_file,'D','Dimensions',{'Ndir',lookup.Ndir,'Nper',lookup.Nper,'NHt',lookup.NHt,'Ny',lookup.Ny},'Datatype','single','Format','classic');
ncwrite(nc_file,'D',lookup.D{1});
ncwriteatt(nc_file,'D','units','degrees');
ncwriteatt(nc_file,'D','longname','Wave direction transformation coefficient');


for w=1:4
    ncwriteatt(nc_file,'/',Name{w},Value{w});
end

% finfo = ncinfo(nc_file_name);
% finfo.Format = 'classic';
% ncwriteschema(nc_file_name,finfo);

end