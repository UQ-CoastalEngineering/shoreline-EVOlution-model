% -- % -- % -- % -- % -- % -- % -- % -- % -- % -- % -- % -- % -- % -- %

% -- % -- % -- % -- % -- % -- % -- % -- % -- % -- % -- % -- % -- % -- %

% clear all; close all
subcase{1}='';
subcase{2}='1';


run_name='profileCsite'


%----%----%----%----%----%----%----%----%----%----%----%----%----%----%----%
% Plot basemap
%----%----%----%----%----%----%----%----%----%----%----%----%----%----%----%
cur_dir=pwd;
cd([run_dir,'/processed/'])
sat_plot('narrabeen')
cd(cur_dir)
hold on

ccc=jet(5);

for ic=1:1
    
    cur_dir=pwd;
    
    run_dir=['./../../runs/',run_name,subcase{ic},'/'];
    
    fun_dir=['./../functions/'];
    addpath(fun_dir)
    
    disp(['Loading'])
    load([run_dir,'/processed/',run_name,'_in.mat']);
    
    load([run_dir,'/processed/',run_name,'_out.mat']);
    
    [nt ny]=size(xshore.X0);
    disp('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~')
    disp(['run ',subcase{ic}])
    datestr(xshore.time(end))
    disp('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~')

    % plot some lines on the map!!!	
for ti=1:1000:nt
    	x0_easting=in.xpar.base_e+in.xpar.u*xshore.X0(ti,:);
    	x0_northing=in.xpar.base_n+in.xpar.v*xshore.X0(ti,:);
plot(x0_easting,x0_northing,'.-','color',ccc(cnt,:)); hold on
	cnt=cnt+1;
end

    [xprofile]=evo_profile(xshore,in,0,1); % FUNCTION all times profile 1

    [volume]=xshore_2_vol(xshore,in);
    
    ccc=jet(ceil(nt/100));
    
    figure(100)
    cnt=1;
    for ti=1:100:nt
    plot(xprofile.X(ti,:),xprofile.Z(ti,:),'.-','color',ccc(cnt,:)); hold on
    cnt=cnt+1;

    end
    
    colormap(ccc)
    

end

