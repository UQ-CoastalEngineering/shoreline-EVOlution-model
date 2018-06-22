% -- % -- % -- % -- % -- % -- % -- % -- % -- % -- % -- % -- % -- % -- %
% Load processed output from mat sturcture and display the inputs     %                                                                   %
% -- % -- % -- % -- % -- % -- % -- % -- % -- % -- % -- % -- % -- % -- %

clear all; close all;
run_name='profileCsite';

subcase{1}='';
subcase{2}='1';


for ic=1:1


run_dir=['./../../runs/',run_name,subcase{ic},'/'];

fun_dir=['./../functions/'];
addpath(fun_dir)


%----%----%----%----%----%----%----%----%----%----%----%----%----%----%----%
% Plot basemap
%----%----%----%----%----%----%----%----%----%----%----%----%----%----%----%
cur_dir=pwd;
cd([run_dir,'/processed/'])
sat_plot('narrabeen')
cd(cur_dir)
hold on

load([run_dir,'/processed/',run_name,'_in.mat'])


% -- % -- % -- % -- % -- % -- % -- % -- % -- % -- % -- % -- % -- % -- %
% grid
% -- % -- % -- % -- % -- % -- % -- % -- % -- % -- % -- % -- % -- % -- %
plot(in.grid.base_e,in.grid.base_n,'bo');
hold on
plot(in.grid.end_e,in.grid.end_n,'go');
plot([in.grid.base_e in.grid.end_e]',[in.grid.base_n in.grid.end_n]','g')
text(in.grid.end_e,in.grid.end_n,num2str((1:length(in.grid.base_n))'),'backgroundcolor','w')
text(in.grid.base_e,in.grid.base_n,num2str((in.grid.chainage')'))

dx=(in.grid.end_e-in.grid.base_e);
dy=(in.grid.end_n-in.grid.base_n);
in.grid.eta=cart2pol(dx,dy)*360/2/pi;% Analytical Slope in Cartesian Convention

[in.grid.v in.grid.u]=pol2cart(in.grid.eta*pi/180,1);
quiver(in.grid.end_e,in.grid.end_n,in.grid.v*500,in.grid.u*500,0,'linewidth',2,'color','g','maxheadsize',2)


% -- % -- % -- % -- % -- % -- % -- % -- % -- % -- % -- % -- % -- % -- %
% icond
% -- % -- % -- % -- % -- % -- % -- % -- % -- % -- % -- % -- % -- % -- %
plot(in.icond.x0_e,in.icond.x0_n,'cs');
plot([in.icond.x0_e,in.icond.x1_e]',[in.icond.x0_n,in.icond.x1_n]','r');
plot(in.icond.x1_e,in.icond.x1_n,'cs');
%axis([min(in.grid.end_e) max(in.grid.base_e) min(in.grid.base_n) max(in.grid.end_n)])


% -- % -- % -- % -- % -- % -- % -- % -- % -- % -- % -- % -- % -- % -- %
% xpar
% -- % -- % -- % -- % -- % -- % -- % -- % -- % -- % -- % -- % -- % -- %
plot(in.xpar.base_e,in.xpar.base_n,'xy','markersize',10,'linewidth',2);

% -- % -- % -- % -- % -- % -- % -- % -- % -- % -- % -- % -- % -- % -- %
% groynes
% -- % -- % -- % -- % -- % -- % -- % -- % -- % -- % -- % -- % -- % -- %
if isfield(in,'groynes')==1
plot([in.groynes.base_e in.groynes.end_e]',[in.groynes.base_n in.groynes.end_n]','gx-','linewidth',3);
end

% -- % -- % -- % -- % -- % -- % -- % -- % -- % -- % -- % -- % -- % -- %
% seawall
% -- % -- % -- % -- % -- % -- % -- % -- % -- % -- % -- % -- % -- % -- %
if isfield(in,'seawall')==1
plot(in.seawall.e,in.seawall.n,'r','linewidth',3);
end

% -- % -- % -- % -- % -- % -- % -- % -- % -- % -- % -- % -- % -- % -- %
% lookup table
% -- % -- % -- % -- % -- % -- % -- % -- % -- % -- % -- % -- % -- % -- %
if isfield(in,'lookup')==1
    in.lookup.end_e=interp1(in.grid.chainage,in.grid.end_e,in.lookup.chainage);
    in.lookup.end_n=interp1(in.grid.chainage,in.grid.end_n,in.lookup.chainage);
    plot(in.lookup.end_e,in.lookup.end_n,'ys','markersize',10,'linewidth',4);
    [in.lookup.v in.lookup.u]=pol2cart(in.lookup.Contour_Rotation*pi/180,1);
    quiver(in.lookup.end_e,in.lookup.end_n,in.lookup.v*500,in.lookup.u*500,0,'linewidth',2,'color','r','maxheadsize',2)
end

xshore.X0=interp1(in.icond.chainage,in.icond.x0,in.xpar.chainage)';
xshore.Z0=interp1(in.icond.chainage,in.icond.z0,in.xpar.chainage)';
xshore.X1=interp1(in.icond.chainage,in.icond.x1,in.xpar.chainage)';
xshore.Z1=interp1(in.icond.chainage,in.icond.z1,in.xpar.chainage)';
xshore.chainage=in.xpar.chainage;

[xprofile]=evo_profile(xshore,in,1,0); % FUNCTION



plot_profiles=1;

if plot_profiles==1
    in.icond.length=interp1(in.grid.chainage,in.grid.length,in.icond.chainage);
    for csi=1:length(in.icond.chainage)
        figure(2); clf
        %show slopes
        plot([0 in.icond.length(csi)],[0 0],'b'); hold on
        plot([0 in.icond.x0(csi)],[xprofile.b_0(csi) in.icond.z0(csi)],'g--');
        plot([0 xprofile.x_transition(csi)],[xprofile.b_1(csi) xprofile.z_transition(csi)],'g--');
        plot([0 in.icond.length(csi)],[xprofile.b_2(csi) in.xpar.z_off(csi)],'g--');
        
        plot(xprofile.X(:,csi),xprofile.Z(:,csi),'r','linewidth',2)
        
        
        % show parameters
        xp=[0;xprofile.x_dune(csi);in.icond.x0(csi);in.icond.x1(csi);xprofile.x_transition(csi);in.icond.length(csi)];
        zp=[in.xpar.z_dune(csi);in.xpar.z_dune(csi);in.icond.z0(csi);in.icond.z1(csi);xprofile.z_transition(csi);in.xpar.z_off(csi)];
        plot(xp,zp,'ko','linewidth',2)
        
%        plot([in.seawall.x(csi) in.seawall.x(csi)],[-10 10],'k','linewidth',3)
        ylim([-10 10])
        orient landscape
        title(['Transect ',num2str(csi)],'fontsize',16)
	pause
    end
    % %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
end

end





