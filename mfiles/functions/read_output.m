function [out]=read_output(out_dir,run_name)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% read outputs % ------------------------------------------------------------------------------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% cross shore %% time, chainage, xwl, zwl, x0, z0, x1, z1---%----%----%----%
variable{1}='xshore';
% shoreline %% time, chainage, x, y%----%----%----%----%----%----%----%----%
variable{2}='shoreline';
% volume %% time, chainage, vol_upper_per_m, vol_per_m, vol, vol_error%----%
variable{3}='vol';
% transport %% time, chainage, thetab, alphab, qspot, qs----%----%----%----%
variable{4}='transport';
% equilibrium cross shore %% time, x z xa za-%----%----%----%----%----%----%
variable{5}='eqxsect';
% cross shore  %% time, x z xa za--%----%----%----%----%----%----%----%----%
variable{6}='xsect';
% wave breaking %% time, chainage, wvht, wvper, wvdir--%----%----%----%----%
variable{7}='wave_brk';
% wave intermediate %% time, chainage, wvht, wvper, wvdir---%----%----%----%
variable{8}='wave_int';
% dune_crest %% time, chainage, x, y%----%----%----%----%----%----%----%----%
variable{9}='dune_crest';
% wave offshore %% time, chainage, wvht, wvper, wvdir--%----%----%----%----%
variable{10}='wave_off';

for i=1:10
    [out_dir,run_name,'_',variable{i},'.csv']
    exist([out_dir,run_name,'_',variable{i},'.csv'])
    if exist([out_dir,run_name,'_',variable{i},'.csv'])>0 & i<10 ;
        statement=['out.',variable{i},'=evo_rd_output(''',out_dir,run_name,'_',variable{i},'.csv'');']
        eval(statement);
        statement=['out.',variable{i},'.variable=','''',variable{i},'''',';'];
        eval(statement);
    elseif exist([out_dir,run_name,'_',variable{i},'.csv'])>0;
        [time,WvHt,WvPer,WvDir]=textread([out_dir,run_name,'_wave_off.csv'],'%s%f%f%f','headerlines',1,'delimiter',',');
        out.wave_off.time=datenum(time,'dd/mm/yyyy HH:MM:SS');
        out.wave_off.WvHt=WvHt;
        out.wave_off.WvPer=WvPer;
        out.wave_off.WvDir=WvDir;
        out.wave_off.variable=variable{9};
    end
end
