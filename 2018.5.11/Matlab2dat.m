function [  ] = Matlab2dat( person_number )
%MATLAB2DAT 此处显示有关此函数的摘要
%   此处显示详细说明
%   onedata传入数据
%   allevents所有的数据的索引
%   allgoodevents所有好的数据的索引
%   person_number是传入的第几个数据
%% read data
if (person_number<10)
    person_number_temp=['0',num2str(person_number)];
else
    person_number_temp=num2str(person_number);
end
filename=['CLAS_VP',person_number_temp,'_onedata_STBFH_MNT.mat'];
load(filename);
%% FFT
for k=1:2
for i=1:80
for j=1:128
Y = fft(onedata(k,j,:,i));
P2 = abs(Y/4000);
P1 = P2(1:4000/2+1);
P1(2:end-1) = 2*P1(2:end-1);
for f=1:400
X(k,j,f,i)=P1(f);
end
end
end
end

for c=1:2
onedata_1 = squeeze(X(c,:,:,:));

j=1;k=1;
%% pick the good trial and pick the trial which all event is 13
for i= 1:80
  if (allevents(1,i)==23) || (allgoodevents(1,i)==0)
   count_1(j)=i;
   j=j+1;
  end
end
%% pick the good trial and pick the trial which all event is 23
for i= 1:80
  if (allevents(1,i)==13) || (allgoodevents(1,i)==0)
   count_2(k)=i;
   k=k+1;
  end
end
onedata_con1=onedata_1;
onedata_con2=onedata_1;
for i=1:(j-1)
    onedata_con1(:,:,count_1(i))=[];
    count_1=count_1-1;
end
for i=1:(k-1)
    onedata_con2(:,:,count_2(i))=[];
    count_2=count_2-1;
end

onedata_all=onedata_con1;
n=size(onedata_all,3);
m=size(onedata_con2,3);

for i=(n+1):(n+m)
onedata_all(:,:,i)=onedata_con2(:,:,i-n);
end
%% average the trials
con1(c,:,:,:)= mean(onedata_con1 ,3);
con2(c,:,:,:)= mean(onedata_con2 ,3);
all(c,:,:,:)= mean(onedata_all ,3);

end
%% save the data to .csv
person_number=num2str(person_number);
filename = ['onedata_',person_number,'_1_1.csv'];
csvwrite (filename,squeeze(con1(1,:,:,:)))

filename = ['onedata_',person_number,'_1_2.csv'];
csvwrite (filename,squeeze(con2(1,:,:,:)))

filename = ['onedata_',person_number,'_1_all.csv'];
csvwrite (filename,squeeze(all(1,:,:,:)))

filename = ['onedata_',person_number,'_2_1.csv'];
csvwrite (filename,squeeze(con1(2,:,:,:)))

filename = ['onedata_',person_number,'_2_2.csv'];
csvwrite (filename,squeeze(con2(2,:,:,:)))

filename = ['onedata_',person_number,'_2_all.csv'];
csvwrite (filename,squeeze(all(2,:,:,:)))

end

