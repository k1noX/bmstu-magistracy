function main()
    % Определение функции чтения изображений из файла
    function y = ImgRead(x)
        img = imread(x);
        
        if size(img, 3) == 3
            img = rgb2gray(img);
        end
        
        [rows, cols] = size(img);
        total_pixels = rows * cols;
        
        img1 = img';
        img2 = reshape(img1, [total_pixels, 1]);
        
        for j = 1:total_pixels
            if img2(j,1) <= 125
                img2(j,1) = 1;
            end
        end
        
        for j = 1:total_pixels
            if img2(j,1) > 125
                img2(j,1) = 0;
            end
        end
        
        y = img2;
    end

    clc; clear;

    %% Обучение модели
    images = cell(33, 1);
    sizes = [];
    for i = 1:33
        letter = char(1039 + i);
        filename = sprintf('..\\data\\alphabet\\%s.png', letter);
        images{i} = ImgRead(filename); % Сохраняем в ячейку
        sizes = [sizes; numel(images{i})];
    end
 
    if length(unique(sizes)) > 1
        error('Все изображения должны иметь одинаковый размер!');
    end
  
    RA = [];
    for i = 1:33
        RA = [RA, images{i}]; % Горизонтальная конкатенация столбцов
    end

    P = double(RA); 
    T = eye(33);
    [R, Q] = size(P);
    [~, S2] = size(T);

    % Создаем зашумленные данные
    P1 = P; 
    T1 = T;
    for i = 1:100
        noise1 = rand(R, Q) * 0.1;
        noise2 = rand(R, Q) * 0.2;
        P1 = [P1, P + noise1, P + noise2];
        T1 = [T1, T, T];
    end

    noisy10 = P(:, 10) + randn(R, 1) * 0.2;

    save('russian_alphabet_data.mat', 'P', 'T', 'P1', 'T1', 'noisy10');
 
    %% Обучение модели
    nnstart;

    %% Построение зашумлённого изображения
    imagesc(reshape(noisy10, [10,16])'), colormap(gray), axis image, axis off, title('Буква Й');
    %% 
    output = sim(results_4.Network, noisy10); % net - экспортированная обученная сеть
    
    % Преобразование выхода в читаемый результат
    result = compet(output);
    [~, answer] = max(result);
    fprintf('Распознанный символ: %s\n', char(1039 + answer));
    imagesc(reshape(P(:, answer), [10,16])'), colormap(gray), axis image, axis off, title(char(1039 + answer));
end