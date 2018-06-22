function [in]=read_input(in_dir,run_name)

% grid %% chainage,base_x,base_y,end_x,end_y-%----%----%----%----%----%----%
if exist([in_dir,run_name,'_grid.csv'])>0
    [in.grid.chainage in.grid.base_e in.grid.base_n in.grid.end_e in.grid.end_n]=...
        textread([in_dir,run_name,'_grid.csv'],'%f%f%f%f%f','delimiter',',','headerlines',1);
    %
    in.grid.length_e=in.grid.end_e-in.grid.base_e; % E-W length
    in.grid.length_n=in.grid.end_n-in.grid.base_n; % N-s length
    in.grid.length=sqrt(in.grid.length_e.^2+in.grid.length_n.^2);
    %     in.grid.dC=[sqrt(diff(in.grid.base_e).^2+diff(in.grid.base_n).^2)];
    %     for qq=1:length(in.grid.dC);
    %         in.grid.C(qq)=sum(in.grid.dC(1:qq));
    %     end
    %     in.grid.C=[0 in.grid.C];
    %
end

% icond %% chainage,x0,z0,x1,z1,dx-%----%----%----%----%----%----%----%----%
if exist([in_dir,run_name,'_icond.csv'])>0
    [in.icond.chainage in.icond.x0 in.icond.z0 in.icond.x1 in.icond.z1 in.icond.dx]=...
        textread([in_dir,run_name,'_icond.csv'],'%f%f%f%f%f%f','delimiter',',','headerlines',1);
    %
    in.icond.base_e=interp1(in.grid.chainage,in.grid.base_e,in.icond.chainage);
    in.icond.base_n=interp1(in.grid.chainage,in.grid.base_n,in.icond.chainage);
    in.icond.u=interp1(in.grid.chainage,in.grid.length_e,in.icond.chainage);
    in.icond.v=interp1(in.grid.chainage,in.grid.length_n,in.icond.chainage);
    in.icond.m=sqrt(in.icond.u.^2+in.icond.v.^2);
    in.icond.u=(in.icond.u)./(in.icond.m);
    in.icond.v=(in.icond.v)./(in.icond.m);
    in.icond.x0_e=in.icond.base_e+(in.icond.x0).*(in.icond.u);
    in.icond.x0_n=in.icond.base_n+(in.icond.x0).*(in.icond.v);
    in.icond.x1_e=in.icond.base_e+(in.icond.x1).*(in.icond.u);
    in.icond.x1_n=in.icond.base_n+(in.icond.x1).*(in.icond.v);
    %
end

% xpar %% chainage,dune_elevation,dune_slope,a,transition_slope,offshore_slope,offshore_elevation,ke,ka,hbmin,description
if exist([in_dir,run_name,'_xpar.csv'])>0
    [in.xpar.chainage in.xpar.z_dune in.xpar.s_dune in.xpar.a in.xpar.s_transition in.xpar.s_off in.xpar.z_off in.xpar.ke in.xpar.ka in.xpar.hb in.xpar.descripton]=...
        textread([in_dir,run_name,'_xpar.csv'],'%f%f%f%f%f%f%f%f%f%f%s','delimiter',',','headerlines',1);
    %
    in.xpar.base_e=interp1(in.grid.chainage,in.grid.base_e,in.xpar.chainage);
    in.xpar.base_n=interp1(in.grid.chainage,in.grid.base_n,in.xpar.chainage);
    in.xpar.u=interp1(in.grid.chainage,in.grid.length_e,in.xpar.chainage);
    in.xpar.v=interp1(in.grid.chainage,in.grid.length_n,in.xpar.chainage);
    in.xpar.m=sqrt(in.xpar.u.^2+in.xpar.v.^2);
    in.xpar.u=(in.xpar.u)./(in.xpar.m);
    in.xpar.v=(in.xpar.v)./(in.xpar.m);
    
    %
end

% groynes %% chainage,length_groyne,start_t,end_t,description
if exist([in_dir,run_name,'_groynes.csv'])>0
    [in.groynes.chainage in.groynes.l in.groynes.t1 in.groynes.t2 in.groynes.descripton]=...
        textread([in_dir,run_name,'_groynes','.csv'],'%f%f%s%s%s','delimiter',',','headerlines',1);
    %
    in.groynes.base_e=interp1(in.grid.chainage,in.grid.base_e,in.groynes.chainage);
    in.groynes.base_n=interp1(in.grid.chainage,in.grid.base_n,in.groynes.chainage);
    in.groynes.u=interp1(in.grid.chainage,in.grid.length_e,in.groynes.chainage);
    in.groynes.v=interp1(in.grid.chainage,in.grid.length_n,in.groynes.chainage);
    in.groynes.m=sqrt(in.groynes.u.^2+in.groynes.v.^2);
    in.groynes.u=(in.groynes.u)./(in.groynes.m);
    in.groynes.v=(in.groynes.v)./(in.groynes.m);
    in.groynes.end_e=in.groynes.base_e+(in.groynes.l).*(in.groynes.u);
    in.groynes.end_n=in.groynes.base_n+(in.groynes.l).*(in.groynes.v);
    %
end


% seawall %% chainage,length_seawall,start_t,end_t,description
if exist([in_dir,run_name,'_seawall.csv'])>0
    [in.seawall.chainage in.seawall.x in.seawall.z]=...
        textread([in_dir,run_name,'_seawall','.csv'],'%f%f%f','delimiter',',','headerlines',1);
    %     in.seawall.x(in.seawall.x==0)=nan;
    %     in.seawall.x(isnan(in.seawall.x))=0;
    
    % for plotting
%     if length(in.seawall.x)<length(in.grid.chainage)
%         seawall.x=zeros(size(in.grid.chainage));
%         seawall.z=zeros(size(in.grid.chainage));;
%         seawall.chainage=in.grid.chainage;
%     end

%     seawall.x(seawall.x==0)=nan;
%     in.seawall.base_e=in.grid.base_e;
%     in.seawall.base_n=in.grid.base_n;
%     in.seawall.m=sqrt(in.grid.length_e.^2+in.grid.length_n.^2);
%     in.seawall.u=(in.grid.length_e)./(in.seawall.m);
%     in.seawall.v=(in.grid.length_n)./(in.seawall.m);
%     in.seawall.e=in.seawall.base_e+(in.seawall.x).*(in.seawall.u);
%     in.seawall.n=in.seawall.base_n+(in.seawall.x).*(in.seawall.v); 
%     
    
    in.seawall.x=interp1(in.seawall.chainage,in.seawall.x,in.xpar.chainage);
    in.seawall.z=interp1(in.seawall.chainage,in.seawall.z,in.xpar.chainage);
    in.seawall.chainage=in.xpar.chainage;
    
%     in.seawall.x(in.seawall.x==0)=nan;
    
    in.seawall.base_e=interp1(in.grid.chainage,in.grid.base_e,in.seawall.chainage);
    in.seawall.base_n=interp1(in.grid.chainage,in.grid.base_n,in.seawall.chainage);
    in.seawall.u=interp1(in.grid.chainage,in.grid.length_e,in.seawall.chainage);
    in.seawall.v=interp1(in.grid.chainage,in.grid.length_n,in.seawall.chainage);
    in.seawall.m=sqrt(in.seawall.u.^2+in.seawall.v.^2);
    in.seawall.u=(in.seawall.u)./(in.seawall.m);
    in.seawall.v=(in.seawall.v)./(in.seawall.m);
    in.seawall.e=in.seawall.base_e+(in.seawall.x).*(in.seawall.u);
    in.seawall.n=in.seawall.base_n+(in.seawall.x).*(in.seawall.v);
    
%     in.seawall.xg=nan(size(in.grid.chainage));
%     ii=find(in.seawall.x>0);
%     in.seawall.xg(ii)=in.seawall.x(ii);
%     in.seawall.xg(ii+1)=in.seawall.x(ii);

 
    
    
end


% wave %% t wvht wvper wvdir
if exist([in_dir,run_name,'_wave.csv'])>0
    [in.wave.t in.wave.hs in.wave.tp in.wave.dp]=...
        textread([in_dir,run_name,'_wave','.csv'],'%s%f%f%f','delimiter',',','headerlines',1);
    in.wave.t=datenum(in.wave.t,'dd/mm/yyyy HH:MM');
end

% wl %% t,wl-%----%----%----%----%----%----%----%----%----%
if exist([in_dir,run_name,'_wl.csv'])>0
    [in.wl.t in.wl.eta]=...
        textread([in_dir,run_name,'_wl.csv'],'%s%f','delimiter',',','headerlines',1);
    in.wl.t=datenum(in.wl.t,'dd/mm/yyyy HH:MM');
end

% lookup %%
if exist([in_dir,run_name,'_lookup.nc'])>0
    fn=[in_dir,run_name,'_lookup','.nc'];
    info=ncinfo(fn);
    for vi=1:length(info.Variables);
        var=info.Variables(vi).Name;
        statement=['in.lookup.',var,'=ncread(''',fn,''',','''',var,'''',');'];
        eval(statement);
    end
    %
    in.lookup.chainage=in.lookup.Chainage;in.lookup = rmfield(in.lookup,'Chainage');
    
    %     in.lookup.base_e=interp1(in.grid.chainage,in.grid.base_e,in.lookup.chainage);
    %     in.lookup.base_n=interp1(in.grid.chainage,in.grid.base_n,in.lookup.chainage);
    %     in.lookup.u=interp1(in.grid.chainage,in.grid.length_e,in.lookup.chainage);
    %     in.lookup.v=interp1(in.grid.chainage,in.grid.length_n,in.lookup.chainage);
    %     in.lookup.m=sqrt(in.lookup.u.^2+in.lookup.v.^2);
    %     in.lookup.u=(in.lookup.u)./(in.lookup.m);
    %     in.lookup.v=(in.lookup.v)./(in.lookup.m);
    
    %     in.lookup.icond.x0=interp1(in.icond.chainage,in.icond.x0,in.lookup.chainage,'linear','extrap');
    %     in.lookup.icond.z0=interp1(in.icond.chainage,in.icond.z0,in.lookup.chainage,'linear','extrap');
    %     in.lookup.icond.x1=interp1(in.icond.chainage,in.icond.x1,in.lookup.chainage,'linear','extrap');
    %     in.lookup.icond.z1=interp1(in.icond.chainage,in.icond.z1,in.lookup.chainage,'linear','extrap');
    %     in.lookup.icond.chainage=in.lookup.chainage;
    
    %     in.lookup.xpar.z_dune=interp1(in.xpar.chainage,in.xpar.z_dune,in.lookup.chainage,'linear','extrap');
    %     in.lookup.xpar.s_dune=interp1(in.xpar.chainage,in.xpar.s_dune,in.lookup.chainage,'linear','extrap');
    %     in.lookup.xpar.a=interp1(in.xpar.chainage,in.xpar.a,in.lookup.chainage,'linear','extrap');
    %     in.lookup.xpar.s_transition=interp1(in.xpar.chainage,in.xpar.s_transition,in.lookup.chainage,'linear','extrap');
    %     in.lookup.xpar.s_off=interp1(in.xpar.chainage,in.xpar.s_off,in.lookup.chainage,'linear','extrap');
    %     in.lookup.xpar.z_off=interp1(in.xpar.chainage,in.xpar.z_off,in.lookup.chainage,'linear','extrap');
    
end

end
