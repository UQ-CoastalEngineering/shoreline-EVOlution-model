% -- % -- % -- % -- % -- % -- % -- % -- % -- % -- % -- % -- % -- % -- %
% Read all output ascii files for EVO case and save to mat structure   %
% -- % -- % -- % -- % -- % -- % -- % -- % -- % -- % -- % -- % -- % -- %

clear all; close all

subcase{1}='';
subcase{2}='1';


run_name='profileCsite'

for ic=1:1

run_dir=['./../../runs/',run_name,subcase{ic},'/']
fun_dir=['./../functions/'];
addpath(fun_dir)

disp(['Processing'])
[out]=read_output([run_dir,'/output/'],run_name);

disp(['Saving'])

save([run_dir,'/processed/',run_name,'_out.mat'], '-struct', 'out','-v7.3' );

end
