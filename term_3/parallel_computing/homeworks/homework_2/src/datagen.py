import numpy as np
import os
import sys
from pathlib import Path
import argparse

def generate_data(output_path: str, size: int, seed: int = 42, block_size: int = 1024**2):
    """
    Генерирует size 32-битных целых чисел и записывает их в файл блоками.
    
    Args:
        output_path: Путь к выходному файлу
        size: Общее количество чисел
        seed: Сид генератора
        block_size: Количество чисел в одном блоке (по умолчанию 1 млн)
    """
    print(f"Generating {size:,} integers in streaming mode...")
    
    Path(output_path).parent.mkdir(parents=True, exist_ok=True)
    
    total_written = 0
    np.random.seed(seed)
    
    with open(output_path, 'wb') as f:
        while total_written < size:
            remaining = size - total_written
            current_block = min(block_size, remaining)
    
            block = np.random.randint(
                low=-2_147_483_648,
                high=2_147_483_647,
                size=current_block,
                dtype=np.int32
            )

            block.tofile(f)
            total_written += current_block

            if total_written % max(1, size // 10) == 0 or total_written == size:
                print(f"  Progress: {total_written / size * 100:.1f}% ({total_written:,} / {size:,})")
    
    with open(output_path, 'rb') as f:
        sample_bytes = f.read(6 * 4)
        sample = np.frombuffer(sample_bytes, dtype=np.int32)
    
    file_size = os.path.getsize(output_path)
    print(f"  File saved: {output_path}")
    print(f"  Size: {file_size:,} bytes ({file_size / (1024**3):.2f} GB)")
    print(f"  Elements: {size:,}")
    print(f"  Sample: {sample}...")

def main():
    parser = argparse.ArgumentParser(
        description="Generate binary data for OpenCL array swap (streaming)"
    )
    parser.add_argument(
        "--size-exp",
        type=int,
        default=23,
        help="Exponent N for array size 2^N (default: 23)"
    )
    parser.add_argument(
        "--output",
        type=str,
        default=f"{os.path.dirname(os.path.abspath(__file__))}/../data/input.bin",
        help="Output file path (default: ../data/input.bin)"
    )
    parser.add_argument(
        "--seed",
        type=int,
        default=42,
        help="Random seed (default: 42)"
    )
    parser.add_argument(
        "--block-size",
        type=int,
        default=1024**2,
        help="Block size in elements (default: 1048576)"
    )
    
    args = parser.parse_args()
    
    size = 2 ** args.size_exp
    
    if size % 2 != 0:
        size -= 1
    
    estimated_gb = (size * 4) / (1024**3)
    if estimated_gb > 1.0:
        print(f"Warning: Output file will be ~{estimated_gb:.1f} GB")
        if input("Continue? (y/N): ").lower() != 'y':
            print("Aborted.")
            sys.exit(0)
    
    try:
        generate_data(args.output, size, args.seed, block_size=args.block_size)
        print("Done!")
    except KeyboardInterrupt:
        print("Interrupted by user")
        sys.exit(1)
    except Exception as e:
        print(f"Error: {e}")
        sys.exit(1)

if __name__ == "__main__":
    main()
