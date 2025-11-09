#!/usr/bin/env python3
"""
Test script to verify the Streamlit dashboard runs without errors
"""

import sys
import subprocess
import time
import requests
from pathlib import Path

def test_streamlit_app():
    """Test that the Streamlit app starts successfully"""
    print("Testing Streamlit dashboard...")
    
    # Check if app.py exists
    app_path = Path("app.py")
    if not app_path.exists():
        print("❌ Error: app.py not found")
        return False
    
    print("✓ app.py found")
    
    # Check if required data directories exist
    data_dir = Path("05_analysis_spatial/03_tables")
    if not data_dir.exists():
        print("⚠️  Warning: Data directory not found:", data_dir)
    else:
        print("✓ Data directory found")
    
    # Check if key CSV files exist
    key_files = [
        "05_analysis_spatial/03_tables/riepilogo_cluster_LISA_completo.csv",
        "05_analysis_spatial/03_tables/riepilogo_cluster_GI_completo.csv",
        "05_analysis_spatial/03_tables/riepilogo_modello_dettaglio.csv",
        "05_analysis_spatial/03_tables/moran_summary_all.csv"
    ]
    
    for file in key_files:
        if Path(file).exists():
            print(f"✓ {Path(file).name} found")
        else:
            print(f"⚠️  {Path(file).name} not found")
    
    # Try to start Streamlit in headless mode
    print("\nStarting Streamlit server...")
    proc = subprocess.Popen(
        ["streamlit", "run", "app.py", "--server.headless", "true", "--server.port", "8502"],
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE
    )
    
    # Wait for server to start
    time.sleep(5)
    
    # Check if server is running
    try:
        response = requests.get("http://localhost:8502", timeout=5)
        if response.status_code == 200:
            print("✓ Streamlit server started successfully")
            proc.terminate()
            proc.wait(timeout=5)
            print("✓ Server stopped cleanly")
            return True
        else:
            print(f"❌ Server returned status code: {response.status_code}")
            proc.terminate()
            return False
    except requests.exceptions.RequestException as e:
        print(f"❌ Error connecting to server: {e}")
        proc.terminate()
        return False
    except Exception as e:
        print(f"❌ Unexpected error: {e}")
        proc.terminate()
        return False

if __name__ == "__main__":
    success = test_streamlit_app()
    if success:
        print("\n✅ All tests passed!")
        sys.exit(0)
    else:
        print("\n❌ Some tests failed")
        sys.exit(1)
