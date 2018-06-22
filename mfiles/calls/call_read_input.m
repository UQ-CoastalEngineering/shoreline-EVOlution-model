% -- % -- % -- % -- % -- % -- % -- % -- % -- % -- % -- % -- % -- % -- %
% Read all input ascii files for EVO case and save to mat structure   %
% -- % -- % -- % -- % -- % -- % -- % -- % -- % -- % -- % -- % -- % -- %

clear all; close all;
run_name='profileCsite';

subcase{1}='';
subcase{2}='1';


for ic=1:1

run_dir=['./../../runs/',run_name,subcase{ic},'/'];

fun_dir=['./../functions/'];


addpath(fun_dir)

disp(['Processing'])
[in]=read_input([run_dir,'/input/'],run_name);

disp(['Saving'])
dumb.in=in;
save([run_dir,'/processed/',run_name,'_in.mat'], '-struct', 'dumb','-v7.3' );
clear dumb

end


