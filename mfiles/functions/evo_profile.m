function [xprofile]=evo_profile(xshore,in,i_t,i_y)
% function [xprofile]=evo_profile(grid,xshore,xpar,seawall,i_t,i_y)
% disp(['evo_profile.m input structure has been modified'])

if isfield(in,'seawall')==0
   in.seawall.x=zeros(size(in.xpar.chainage));
   in.seawall.z=zeros(size(in.xpar.chainage));
end


na=25;
% xprofile.base=zeros(size(xshore.chainage));
% xprofile.end=interp1(in.grid.chainage,in.grid.length,xshore.chainage);

if i_t == 0
    %disp('Processing specified profile for all times')
    
    nt=length(xshore.Z0(:,1));
    ny=1;
    
    % 0 (scalars)
    xprofile.base=0;
    xprofile.end=interp1(in.grid.chainage,in.grid.length,xshore.chainage(i_y));
    xprofile.end=in.xpar.m(i_y);%interp1(in.grid.chainage,in.grid.length,xshore.chainage(i_y));
    % 1
    b_0=xshore.Z0(:,i_y)+in.xpar.s_dune(i_y)*xshore.X0(:,i_y);
    
    % 2
    x_dune=(-b_0+in.xpar.z_dune(i_y))./-in.xpar.s_dune(i_y);
    
    % 3
    a=(xshore.Z0(:,i_y)-xshore.Z1(:,i_y))./((xshore.X1(:,i_y)-xshore.X0(:,i_y)).^(2/3));
    
    % 4
    % dx=(icond.X1-icond.X0)/na;
    
    % 5
    b_1=xshore.Z1(:,i_y)+in.xpar.s_transition(i_y)*xshore.X1(:,i_y);
    
    % 6 (scalar)
    b_2=(in.xpar.z_off(i_y)+in.xpar.s_off(i_y)*xprofile.end);
    
    % 7
    x_transition=(b_2-b_1)/(-in.xpar.s_transition(i_y)+in.xpar.s_off(i_y));
    
    % 8
    z_transition=-in.xpar.s_off(i_y)*x_transition+b_2;

    xprofile.x_dune=x_dune;
    xprofile.x_transition=x_transition;
    xprofile.z_transition=z_transition;
    xprofile.b_0=b_0;
    xprofile.b_1=b_1;
    xprofile.b_2=b_2;
    xprofile.a=a;
    
    %dx=(xprofile.end-xprofile.base)/101;
    
    wstar=xshore.X1(:,i_y)-xshore.X0(:,i_y);
    nap=20;
    
    xprofile.X=[ones(nt,1)*xprofile.base,x_dune,xshore.X0(:,i_y)*ones(1,nap+1)+((wstar/nap)*(0:nap)),x_transition,ones(nt,1)*xprofile.end];
    zz=xshore.Z0(:,i_y)*ones(1,nap+1)-(a*ones(1,nap+1)).*(((wstar/nap)*(0:nap)).^(2/3));
    xprofile.Z=[ones(nt,1)*in.xpar.z_dune(i_y),ones(nt,1)*in.xpar.z_dune(i_y),zz,z_transition,ones(nt,1)*in.xpar.z_off(i_y)];
    %xprofile.z(xprofile.x <= in.sewall.x(i_y) & xprofile.z <= in.sewall.z(i_y))=in.sewall.z(i_y);
    
elseif i_y==0
    
    %disp('Processing specified time for all profiles')
    
    ny=length(xshore.Z0(1,:));
    nt=1;
    % 0 
    xprofile.base=zeros(nt,ny);
    xprofile.end=in.xpar.m';%xprofile.end=interp1(in.grid.chainage,in.grid.length,xshore.chainage)';
    
    % 1
    b_0=xshore.Z0(i_t,:)+in.xpar.s_dune'.*xshore.X0(i_t,:);
    
    % 2
    x_dune=(-b_0+in.xpar.z_dune')./-in.xpar.s_dune';
    
    % 3
    a=(xshore.Z0(i_t,:)-xshore.Z1(i_t,:))./((xshore.X1(i_t,:)-xshore.X0(i_t,:)).^(2/3));
    
    % 4
    % dx=(icond.X1-icond.X0)/na;
    
    % 5
    b_1=xshore.Z1(i_t,:)+in.xpar.s_transition'.*xshore.X1(i_t,:);

    % 6 (scalar)
    b_2=in.xpar.z_off'+in.xpar.s_off'.*xprofile.end;
    
    % 7
    x_transition=(b_2-b_1)./(-in.xpar.s_transition'+in.xpar.s_off');
    
    % 8
    z_transition=-in.xpar.s_off'.*x_transition+b_2;
    
    xprofile.x_dune=x_dune;
    xprofile.x_transition=x_transition;
    xprofile.z_transition=z_transition;
    xprofile.b_0=b_0;
    xprofile.b_1=b_1;
    xprofile.b_2=b_2;
    
    dx=(xprofile.end-xprofile.base)/101;
    
    wstar=xshore.X1(i_t,:)-xshore.X0(i_t,:);
    nap=20;
    
    xprofile.X=[xprofile.base;x_dune;ones(nap+1,1)*xshore.X0(i_t,:)+((0:nap)'*(wstar/nap));x_transition;xprofile.end];
    zz=ones(nap+1,1)*xshore.Z0(i_t,:)-(ones(nap+1,1)*a).*(((0:nap)'*(wstar/nap)).^(2/3));
    xprofile.Z=[in.xpar.z_dune';in.xpar.z_dune';zz;z_transition;in.xpar.z_off'];
end
end

